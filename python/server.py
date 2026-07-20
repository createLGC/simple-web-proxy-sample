from __future__ import annotations
import asyncio, ssl, traceback
from datetime import datetime, timezone
from typing import Optional
from urllib.parse import urlparse

import cert, config, har, ipc, websocket
from httpbase import HTTPContentType, HTTPHeaders, HTTPBody
from http1 import HTTP1Request, HTTP1Response

def create_HAR_entry(startedDateTime: str, clientIPAddress: str, serverIPAddress: str, request: har.HAR_request, response: har.HAR_response) -> har.HAR_entry:
    return har.HAR_entry(None, startedDateTime, 0, request, response, har.HAR_cache(None, None, None), har.HAR_timings(None, None, None, 0, 0, 0, None, None), clientIPAddress, serverIPAddress, None, None)

async def transparent_relay(client_reader: asyncio.StreamReader, client_writer: asyncio.StreamWriter, server_reader: asyncio.StreamReader, server_writer: asyncio.StreamWriter, config_obj: config.ConfigObject, does_unproxify: bool, request: Optional[HTTP1Request]):
    clientIPAddresses = client_writer.get_extra_info('peername')
    clientIPAddress = clientIPAddresses[0] if clientIPAddresses else None

    serverIPAddresses = server_writer.get_extra_info('peername')
    serverIPAddress = serverIPAddresses[0] if serverIPAddresses else None

    isWebSocket = False

    while True:
        startedDateTime = datetime.now(timezone.utc).isoformat(timespec="seconds")

        if not request:
            request = await HTTP1Request.parse(client_reader, client_writer)
        
        host = request.host
        proxied = request.proxied
        encrypted = not proxied
        
        if proxied and does_unproxify:
            request.unproxify()

        modifyRequest = config_obj.requestModifier
        modified_request = modifyRequest(host, request) if modifyRequest else request
        await modified_request.send(server_writer)

        response = await HTTP1Response.parse(server_reader)

        modifyResponse = config_obj.responseModifier
        modified_response = modifyResponse(host, modified_request, response) if modifyResponse else response
        await modified_response.send(client_writer)

        await ipc.sendEntry(create_HAR_entry(startedDateTime, clientIPAddress, serverIPAddress, modified_request.to_har(encrypted), modified_response.to_har()))

        request = None

        if modified_response.status_code == 101 and modified_response.headers.upgrade_websocket:
            isWebSocket = True
            break

    if isWebSocket:
        await asyncio.gather(websocket_relay(client_reader, server_writer), websocket_relay(server_reader, client_writer))

async def opaque_relay(src: asyncio.StreamReader, dst: asyncio.StreamWriter, buf_size: int = 4096) -> None:
    while True:
        buffer = b""
        while True:
            data = await src.read(buf_size)
            buffer += data
            if len(data) < buf_size:
                break
        dst.write(buffer)
        await dst.drain()
        await asyncio.sleep(0.2)

async def websocket_relay(src: asyncio.StreamReader, dst: asyncio.StreamWriter) -> None:
    while True:
        frame = await websocket.Frame.parse(src)
        dst.write(frame.to_bytes())

async def handshake_with_proxy(proxy_reader: asyncio.StreamReader, proxy_writer: asyncio.StreamWriter, server_address: tuple[str, int]) -> None:
    host, port = server_address
    proxy_writer.write(f"CONNECT {host}:{port} HTTP/1.1\r\n\r\n".encode())
    await proxy_writer.drain()
    response = await HTTP1Response.parse(proxy_reader)
    if not (200 <= response.status_code < 300):
        proxy_writer.close()
        await proxy_writer.wait_closed()
        raise OSError(f"Tunnel connection failed: {response.status_code} {response.status_text}")

if not hasattr(asyncio.StreamWriter, "start_tls"):
    async def start_tls(self: asyncio.StreamWriter, sslcontext, *,
                        server_hostname=None,
                        ssl_handshake_timeout=None,
                        #ssl_shutdown_timeout=None
    ):
        """Upgrade an existing stream-based connection to TLS."""
        server_side = self._protocol._client_connected_cb is not None
        protocol = self._protocol
        await self.drain()
        new_transport = await self._loop.start_tls(  # type: ignore
            self._transport, protocol, sslcontext,
            server_side=server_side, server_hostname=server_hostname,
            ssl_handshake_timeout=ssl_handshake_timeout,
            #ssl_shutdown_timeout=ssl_shutdown_timeout
        )
        self._transport = new_transport
        protocol._transport = new_transport
        protocol._over_ssl = new_transport.get_extra_info('sslcontext') is not None
    asyncio.StreamWriter.start_tls = start_tls

async def wrap_client_with_SSLSocket(client: asyncio.StreamWriter, host: str) -> None:
    certpath, keypath, password = cert.get(host)
    context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    context.load_cert_chain(certpath, keypath, password)
    context.set_alpn_protocols(['http/1.1'])
    await client.start_tls(context)

async def wrap_server_with_SSLSocket(server: asyncio.StreamWriter, host: str) -> None:
    context = ssl.create_default_context()
    context.set_alpn_protocols(['http/1.1'])
    await server.start_tls(context, server_hostname=host)

def create_error_response(error: Exception) -> HTTP1Response:
    error_html = f"""
<!DOCTYPE html>
<html>
    <head>
        <title>Handshake Failed</title>
    </head>
    <body>
        <h1>Handshake Failed</h1>
        <pre>{error}</pre> 
    </body>
</html>
"""
    error_html_bytes = error_html.encode()
    error_response_headers = HTTPHeaders([
        ('Connection', 'close'),
        ('Proxy-Connection', 'close'),
        ('Content-Type', 'text/html'),
        ('Content-Length', len(error_html_bytes))
    ])
    error_response_body = HTTPBody(error_html_bytes, None, HTTPContentType.parse('text/html'))
    error_response = HTTP1Response(error_response_headers, error_response_body, 'HTTP/1.1', 400, "Bad Request")
    return error_response 

async def handle_request(client_reader: asyncio.StreamReader, client_writer: asyncio.StreamWriter):
    try:
        try:
            config_obj = config.load()
            client_address = client_writer.get_extra_info('peername')
            if config_obj and not config_obj.judgeAcceptable(client_address[0]):
                global _permission_request
                if _permission_request:
                    await ipc.requestPermission(client_address[0])
                raise ConnectionError(f"This client is not permitted to connect. {client_address[0]}")
        
            startedDateTime = datetime.now(timezone.utc).isoformat(timespec="seconds")

            request = await HTTP1Request.parse(client_reader, client_writer)

            tunnel = request.method == "CONNECT"

            if tunnel:
                server_address = request.path.split(':')
            else:
                parse_result = urlparse(request.path)
                netloc = parse_result.netloc.split(':')
                host = netloc[0]
                port = netloc[1] if len(netloc) > 1 else "80"
                server_address = (host, port)
            
            proxy_address = None
            if config_obj and (getExternalProxy := config_obj.getExternalProxy) and (proxy_address := getExternalProxy(client_address[0], server_address[0])):
                server_reader, server_writer = await asyncio.open_connection(proxy_address[0], proxy_address[1])
                if tunnel:
                    await handshake_with_proxy(server_reader, server_writer, server_address)
            else:
                server_reader, server_writer = await asyncio.open_connection(server_address[0], server_address[1])
        except Exception as e:
            await create_error_response(e).send(client_writer)
            raise e

        if tunnel:
            success_response = HTTP1Response(HTTPHeaders(), HTTPBody(), "HTTP/1.1", 200, "Connection established")
            await success_response.send(client_writer)

            if config_obj and (judgeSslProxying := config_obj.judgeSslProxying) and judgeSslProxying(client_address[0], server_address[0]):
                await wrap_client_with_SSLSocket(client_writer, server_address[0])
                await wrap_server_with_SSLSocket(server_writer, server_address[0])
                await transparent_relay(client_reader, client_writer, server_reader, server_writer, config_obj, False, None)
            else:
                entry = create_HAR_entry(startedDateTime, client_address[0], server_address[0], request.to_har(), success_response.to_har())
                await asyncio.gather(opaque_relay(client_reader, server_writer), opaque_relay(server_reader, client_writer), ipc.sendEntry(entry))
        else:
            await transparent_relay(client_reader, client_writer, server_reader, server_writer, config_obj, proxy_address is None, request)
    except (ConnectionResetError, BrokenPipeError, asyncio.exceptions.IncompleteReadError, GeneratorExit, KeyboardInterrupt):
        pass
    except:
        if "request" in locals():
            print(request.path, flush=True)
        traceback.print_exc()
    finally:
        client_writer.close()
        if "server_writer" in locals():
            server_writer.close()

async def start_server(address: tuple[str, int]):
    host, port = address
    server = await asyncio.start_server(handle_request, host, port)

    try:
        async with server:
            await server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.close()
        await server.wait_closed()
        print("server stopped")

_permission_request = False

def setup(permission_request: bool):
    global _permission_request
    _permission_request = permission_request
from __future__ import annotations
import asyncio, zlib
from dataclasses import dataclass
from urllib.parse import urlparse

from httpbase import HTTPBody, HTTPHeaders, HTTPQueries, HTTPValueWithOptions, compress_with_codings, decompress_with_codings
import har

async def parse_headers(reader: asyncio.StreamReader) -> HTTPHeaders:
    headers = HTTPHeaders()

    while (header_line := (await reader.readuntil(separator=b'\r\n')).decode().rstrip("\r\n")) != "":
        headers.append(tuple(item.strip() for item in header_line.split(':', 1)))
    
    return headers

async def send_headers(writer, headers) -> None:
    for header in headers:
        writer.write(f"{header[0]}: {header[1]}\r\n".encode())
    writer.write(b"\r\n")
    await writer.drain()

async def parse_transfer_encoding(reader: asyncio.StreamReader, transfer_codings: list[str]) -> bytes:
    # https://www.rfc-editor.org/rfc/rfc9112#section-6.1
    if 'chunked' in transfer_codings:
        content = b""
        while True:
            chunk_header = (await reader.readuntil(separator=b'\r\n')).decode().rstrip("\r\n")
            if chunk_header == "": continue
            chunk_size = int(HTTPValueWithOptions.parse(chunk_header).value, 16)
            if chunk_size == 0: break
            content += await reader.readexactly(chunk_size)
            await reader.readuntil(separator=b'\r\n')
        trailer = await parse_headers(reader)
        transfer_codings.remove('chunked')
        if transfer_codings:
            return decompress_with_codings(content, transfer_codings)
        else:
            return content
    else:
        raise NotImplementedError("Not Chunked Transfer-Encoding")

async def send_transfer_encoding(writer: asyncio.StreamWriter, data: bytes, transfer_codings: list[str], buf_size: int = 256) -> None:
    if 'chunked' in transfer_codings:
        transfer_codings.remove('chunked')
        if transfer_codings:
            content = compress_with_codings(data, transfer_codings)
        else:
            content = data
        i = 0
        content_size = len(content)
        while i < content_size:
            chunk_data = content[i:i+buf_size]
            chunk_size = len(chunk_data)
            chunk_header = f"{hex(chunk_size)[2:]}\r\n".encode()
            writer.write(chunk_header)
            writer.write(chunk_data)
            writer.write(b"\r\n")
            i += chunk_size
        writer.write(b"0\r\n\r\n")
        await writer.drain()
    else:
        raise NotImplementedError("Not Chunked Transfer-Encoding")

@dataclass
class BaseHTTP1Message:
    headers: HTTPHeaders
    body: HTTPBody

    @property
    def first_line(self) -> str:
        raise NotImplementedError("first_line is not implemented")
    
    def __str__(self) -> str:
        return f"{self.first_line}{self.headers}{self.body}"

    async def send(self, writer: asyncio.StreamWriter) -> None:
        writer.write(self.first_line.encode())
        if self.body.value:
            body = self.body.serialized

            if transfer_codings := self.headers.transfer_encoding:
                await send_headers(writer, self.headers)
                await send_transfer_encoding(writer, body, transfer_codings)
            else:
                self.headers.replace('Content-Length', str(len(body)))
                await send_headers(writer, self.headers)
                writer.write(body)
                await writer.drain()
        else:
            await send_headers(writer, self.headers)

@dataclass
class HTTP1Request(BaseHTTP1Message):
    method: str
    path: str
    queries: HTTPQueries
    version: str

    @classmethod
    async def parse(cls, reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> HTTP1Request:
        request_line = (await reader.readuntil(separator=b'\r\n')).decode().rstrip("\r\n")
        
        method, url, version = request_line.split(' ', 2)
            
        parsed_url = url.split('?', 1)
        path = parsed_url[0]
        queries = HTTPQueries.parse(parsed_url[1]) if len(parsed_url) == 2 else HTTPQueries()

        headers = await parse_headers(reader)

        if accept_encoding := headers.accept_encoding:
            filtered_accept_encoding = list(filter(lambda coding: coding in ['gzip', 'deflate'], accept_encoding))
            headers.replace('Accept-Encoding', ', '.join(filtered_accept_encoding))

        if transfer_codings := headers.transfer_encoding:
            body = await parse_transfer_encoding(reader, transfer_codings)
            if body == None:
                writer.write(b"HTTP/1.1 400 Bad Request\r\n\r\n")
                await writer.drain()
                raise OSError("Failed to parse Transfer Encoding")
        elif content_length := headers.content_length:
            body = await reader.readexactly(content_length)
        else:
            body = None
        try:
            body = HTTPBody(body, headers.content_encoding, headers.content_type)
        except zlib.error as e:
            print(f"{method} {url}", flush=True)
            print(headers, flush=True)
            raise e
        
        return HTTP1Request(headers, body, method, path, queries, version)
    
    @property
    def first_line(self) -> str:
        return f"{self.method} {self.fullpath} {self.version}\r\n"
    
    @property
    def fullpath(self) -> str:
        if self.queries:
            return f"{self.path}?{self.queries}"
        else:
            return self.path
    
    @property
    def host(self) -> str:
        return self.headers.get('Host') or urlparse(self.path if self.proxied or self.path.startswith('/') else f"https://{self.path}").netloc.split(':')[0]

    @property
    def proxied(self) -> bool:
        return self.path.startswith("http://")
    
    def unproxify(self) -> None:
        parse_result = urlparse(self.path)
        path = parse_result.path
        query = parse_result.query
        self.path = f"{path}?{query}" if query else path
    
    def to_har(self, encrypted: bool = True) -> har.HAR_request:
        method = self.method
        if method == "CONNECT":
            url = f"https://{self.path}"
        elif self.proxied:
            url = self.fullpath
        else:
            url = f"{'https' if encrypted else 'http'}://{self.host}{self.fullpath}"
        httpVersion = self.version
        cookies = self.headers.create_HAR_request_cookies()
        headers = [har.HAR_header(item[0], item[1], None) for item in self.headers]
        queryString = [har.HAR_queryString_item(item[0], item[1], None) for item in self.queries]
        postData = self.body.create_HAR_request_postData()
        headersSize = len(str(self.headers))
        bodySize = len(self.body.serialized) if self.body.value else 0
        return har.HAR_request(method, url, httpVersion, cookies, headers, queryString, postData, headersSize, bodySize, None)

@dataclass
class HTTP1Response(BaseHTTP1Message):
    version: str
    status_code: int
    status_text: str

    @classmethod
    async def parse(cls, reader: asyncio.StreamReader) -> HTTP1Response:
        response_line = (await reader.readuntil(separator=b'\r\n')).decode().rstrip("\r\n")

        version, status_code_text, status_text = response_line.split(' ', 2)
        status_code = int(status_code_text)
            
        headers = await parse_headers(reader)

        if transfer_codings := headers.transfer_encoding:
            body = await parse_transfer_encoding(reader, transfer_codings)
        elif content_length := headers.content_length:
            body = await reader.readexactly(content_length)
        else:
            body = None
            
        body = HTTPBody(body, headers.content_encoding, headers.content_type)

        return HTTP1Response(headers, body, version, status_code, status_text)
    
    @property
    def first_line(self) -> str:
        return f"{self.version} {self.status_code} {self.status_text}\r\n"

    def to_har(self) -> har.HAR_response:
        status = self.status_code
        statusText = self.status_text
        httpVersion = self.version
        cookies = self.headers.create_HAR_response_cookies()
        headers = [har.HAR_header(item[0], item[1], None) for item in self.headers]
        content = self.body.create_HAR_content()
        redirectURL = self.headers.get('Location') or ""
        headersSize = len(str(self.headers))
        bodySize = len(self.body.serialized) if self.body.value else 0
        return har.HAR_response(status, statusText, httpVersion, cookies, headers, content, redirectURL, headersSize, bodySize, None)

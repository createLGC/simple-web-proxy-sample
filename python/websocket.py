from __future__ import annotations
import asyncio, base64, hashlib, traceback
from enum import IntEnum
from typing import Optional

from httpbase import HTTPHeaders
from http1 import HTTP1Request, HTTP1Response

async def send_400(writer: asyncio.StreamWriter) -> None:
    response = HTTP1Response(
        version="HTTP/1.1",
        status_code=400,
        status_text="Bad Request",
        headers=HTTPHeaders([("Connection", "close")])
    )
    writer.write(response)
    await writer.drain()
    writer.close()

async def handshake(reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> bool:
    try:
        request = await HTTP1Request.parse(reader, writer)
        swk = request.headers.get('Sec-WebSocket-Key')
        swka = swk + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
        swkaSha1 = hashlib.sha1(swka.encode()).digest()
        swkaSha1Base64 = base64.b64encode(swkaSha1)

        response = HTTP1Response(
            version="HTTP/1.1",
            status_code=101,
            status_text="Switching Protocols",
            headers=HTTPHeaders([
                ("Access-Control-Allow-Origin", "*"),
                ("Connection", "Upgrade"),
                ("Upgrade", "websocket"),
                ("Sec-WebSocket-Accept", swkaSha1Base64)
            ])
        )
        await response.send(writer)
        return True
    except:
        traceback.print_exc()
        await send_400(writer)
        return False

class Opcode(IntEnum):
    CONTINUATION_FRAME = 0
    TEXT_FRAME = 1
    BINARY_FRAME = 2
    CONNECTION_CLOSE = 8
    PING = 9
    PONG = 10

class Frame:
    @classmethod
    async def parse(cls, reader: asyncio.StreamReader) -> Frame:
        first_bytes = await reader.readexactly(2)
        fin = (first_bytes[0] & 0b10000000) != 0
        opcode = Opcode(first_bytes[0] & 0b00001111)
        mask = (first_bytes[1] & 0b10000000) != 0
        msglen = first_bytes[1] & 0b01111111

        if msglen == 126:
            msglen_bytes = await reader.readexactly(2)
            msglen = int.from_bytes(msglen_bytes, byteorder='big')
        elif msglen == 127:
            msglen_bytes = await reader.readexactly(8)
            msglen = int.from_bytes(msglen_bytes, byteorder='big')
        
        masking_key = await reader.readexactly(4) if mask else None
        payload = await reader.readexactly(msglen)
        
        return Frame(fin, opcode, mask, msglen, masking_key, payload)

    def __init__(self, fin: bool, opcode: Opcode, mask: bool, payload_length: int, masking_key: bytes, payload: bytes) -> None:
        self.fin = fin
        self.opcode = opcode
        self.mask = mask
        self.payload_length = payload_length
        self.masking_key = masking_key
        self.payload = payload
    
    @property
    def demasked_payload(self) -> bytes:
        assert self.mask, "payload is not masked"
        decoded = b""
        for i in range(self.payload_length):
            decoded += (self.payload[i] ^ self.masking_key[i % 4]).to_bytes(1)
        return decoded
    
    @property
    def close_payload(self) -> tuple[int, str]:
        assert self.opcode == Opcode.CONNECTION_CLOSE, "opcode is not CONNECTION_CLOSE"
        status_code = int.from_bytes(self.payload[:2], byteorder='big')
        reason = self.payload[2:].decode()
        return (status_code, reason)
    
    def to_bytes(self) -> bytes:
        buffer = b""

        buffer += ((0b10000000 if self.fin else 0b00000000) | self.opcode.value).to_bytes(1)
    
        if self.payload_length <= 125:
            buffer += ((0b10000000 if self.mask else 0b00000000) | self.payload_length).to_bytes(1)
        elif self.payload_length < 2 ** (8 * 2):
            buffer += ((0b10000000 if self.mask else 0b00000000) | 126).to_bytes(1)
            buffer += self.payload_length.to_bytes(2, byteorder='big')
        else:
            buffer += ((0b10000000 if self.mask else 0b00000000) | 127).to_bytes(1)
            buffer += self.payload_length.to_bytes(8, byteorder='big')
        
        if self.mask:
            buffer += self.masking_key
        buffer += self.payload
        
        return buffer

PAYLOAD_LIMIT = 2 ** (8 * 8 - 1) - 1

async def send(opcode: Opcode, data: Optional[bytes], writer: asyncio.StreamWriter) -> None:
    buffer = b""
    msglen = len(data) if data else 0

    fin = msglen <= PAYLOAD_LIMIT

    buffer += ((0b10000000 if fin else 0b00000000) | opcode.value).to_bytes(1)
    
    if fin:
        if msglen <= 125:
            buffer += msglen.to_bytes(1)
        elif msglen < 2 ** (8 * 2):
            buffer += (126).to_bytes(1)
            buffer += msglen.to_bytes(2, 'big')
        else:
            buffer += (127).to_bytes(1)
            buffer += msglen.to_bytes(8, 'big')
        
        buffer += data
        writer.write(buffer)
        await writer.drain()
    else:
        buffer += (127).to_bytes(1)
        buffer += PAYLOAD_LIMIT.to_bytes(8, 'big')
        buffer += data[:PAYLOAD_LIMIT]
        writer.write(buffer)
        await writer.drain()
        await send(Opcode.CONTINUATION_FRAME, data[PAYLOAD_LIMIT:], writer)

async def send_close(writer: asyncio.StreamWriter, status_code: int = 1000, reason: str = "") -> None:
    payload = status_code.to_bytes(2, 'big') + reason.encode()
    await send(Opcode.CONNECTION_CLOSE, payload, writer)

async def read_frame(reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> Frame | None:
    frame = await Frame.parse(reader)

    if frame.opcode == Opcode.CONNECTION_CLOSE:
        await send_close(writer)
        writer.close()
        return
    
    if not frame.mask:
        await send_close(writer, status_code=1002, reason="message is not masked.")
        return await read_frame(reader, writer)
    
    return frame

async def receive(reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> str | bytes | None:
    if frame := await read_frame(reader, writer):
        demasked_payload = frame.demasked_payload

        if not frame.fin:
            while next_frame := await read_frame(reader, writer):
                demasked_payload += next_frame.demasked_payload
                if next_frame.fin:
                    break
            else:
                return
        
        return demasked_payload.decode() if frame.opcode == Opcode.TEXT_FRAME else demasked_payload

async def handle_request(reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
    if await handshake(reader, writer):
        data = await receive(reader, writer)

async def start_server():
    server = await asyncio.start_server(handle_request, "localhost", 8000)

    async with server:
        await server.serve_forever()
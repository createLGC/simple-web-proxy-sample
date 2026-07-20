from __future__ import annotations
from asyncio import gather, StreamReader, StreamWriter
from dataclasses import dataclass
from enum import IntEnum
from typing import Optional
import hpack

@dataclass
class HTTP2Frame:
    class Type(IntEnum):
        DATA = 0
        HEADERS = 1
        PRIORITY = 2
        RST_STREAM = 3
        SETTINGS = 4
        PUSH_PROMISE = 5
        PING = 6
        GOAWAY = 7
        WINDOW_UPDATE = 8
        CONTINUATION = 9

    length: int
    type: Type
    flags: int
    stream_identifier: int
    payload: bytes

    @classmethod
    async def parse(cls, reader: StreamReader) -> Optional[HTTP2Frame]:
        header = await reader.readexactly(9)
        length = int.from_bytes(header[0:3], 'big')
        try:
            type = HTTP2Frame.Type(header[3])
        except Exception as e:
            print(f"frame type error {length}")
            raise e
        flags = header[4]
        stream_identifier = int.from_bytes(header[5:], 'big') #Reserved(0x0, 1bit)も含む
        payload = await reader.readexactly(length)
        return HTTP2Frame(length, type, flags, stream_identifier, payload)
    
    async def send(self, writer: StreamWriter):
        writer.write(self.length.to_bytes(3, 'big'))
        writer.write(self.type.value.to_bytes(1, 'big'))
        writer.write(self.flags.to_bytes(1, 'big'))
        writer.write(self.stream_identifier.to_bytes(4, 'big'))
        writer.write(self.payload)
        await writer.drain()
    
    def __str__(self) -> str:
        return f"length: {self.length}, type: {self.type}, flags: {bin(self.flags)[2:]}, stream_identifier: {bin(self.stream_identifier)[2:]}"

hpack_tables = {}

async def parse_http2_request(client_reader: StreamReader, client_writer: StreamWriter, server_reader: StreamReader, server_writer: StreamWriter, host: str):
    PREFACE = b"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    preface = await client_reader.read(24)

    if preface != PREFACE:
        raise OSError("can't load connection preface")
    
    server_writer.write(PREFACE)

    table = hpack_tables.get(host)
    if not table:
        table = hpack.Table()
        hpack_tables[host] = table
    
    await gather(relay(client_reader, server_writer, table), relay(server_reader, client_writer, table))

async def relay(src: StreamReader, dst: StreamWriter, table: hpack.Table):
    while True:
        frame = await HTTP2Frame.parse(src)
        print(frame)
        """if frame.type == HTTP2FrameType.HEADERS:
            try:
                print(hpack.decode(frame.payload, table))
            except:
                traceback.print_exc()"""
        await frame.send(dst)
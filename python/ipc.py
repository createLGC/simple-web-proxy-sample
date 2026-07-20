import asyncio, har, json
from dataclasses import dataclass, asdict

@dataclass
class IpcMessage:
    type: str
    content: str

_open_connection = None

def setup(address: tuple[str, int]) -> None:
    host, port = address
    global _open_connection
    _open_connection = lambda: asyncio.open_connection(host, port)

async def send(message: str) -> None:
    if not _open_connection: return
    _, writer = await _open_connection()
    writer.write(message.encode())
    await writer.drain()
    writer.close()

async def sendAndGet(message: str) -> str:
    if not _open_connection: return
    reader, writer = await _open_connection()
    writer.write(message.encode())
    response = (await reader.read()).decode()
    await writer.drain()
    writer.close()
    return response

async def sendEntry(entry: har.HAR_entry) -> None:
    data = json.dumps(asdict(IpcMessage("entry", json.dumps(asdict(entry), ensure_ascii=False))), ensure_ascii=False)
    await send(data)

async def requestPermission(client_address: str) -> None:
    data = json.dumps(asdict(IpcMessage("permissionRequest", client_address)))
    await send(data)

async def breakRequest(request: har.HAR_request) -> har.HAR_request:
    data = json.dumps(asdict(IpcMessage("breakRequest", json.dumps(asdict(request), ensure_ascii=False))), ensure_ascii=False)
    return har.HAR_request.from_json(await sendAndGet(data))

async def breakResponse(response: har.HAR_response) -> har.HAR_response:
    data = json.dumps(asdict(IpcMessage("breakRequest", json.dumps(asdict(response), ensure_ascii=False))), ensure_ascii=False)
    return har.HAR_response.from_json(await sendAndGet(data))
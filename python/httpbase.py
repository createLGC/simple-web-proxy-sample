from __future__ import annotations
import base64, functools, json
from dataclasses import dataclass
from io import BytesIO
from http.cookies import SimpleCookie
from typing import Optional, Union
from urllib.parse import parse_qsl, urlencode

import har
from encoding import compress_with_codings, decompress_with_codings

class HTTPQueries(list):
    @classmethod
    def parse(cls, queryString: str) -> HTTPQueries:
        return HTTPQueries(parse_qsl(queryString, keep_blank_values=True))

    def __str__(self) -> str:
        return urlencode(self)

    def get(self, name: str) -> Optional[str]:
        value = None
        for item in self:
            if item[0] == name:
                value = item[1]
        return value
    
    def getall(self, name: str) -> list[str]:
        return [item[1] for item in filter(lambda item: item[0] == name, self)]
    
    def delete(self, name: str) -> None:
        for i in reversed([i for i, item in enumerate(self) if item[0] == name]):
            del self[i]
    
    def replace(self, name: str, value: str) -> None:
        self.delete(name)
        self.append((name, value))

@dataclass
class HTTPValueWithOptions:
    value: str
    options: list[tuple[str, str]]

    @classmethod
    def parse(cls, string: str) -> HTTPValueWithOptions:
        value = ""
        options = []
        for i, item in enumerate(string.split(';')):
            if i == 0:
                value = item.strip()
                continue
            try:
                opt_key, opt_value = [opt_item.strip() for opt_item in item.split('=')]
                options.append((opt_key, opt_value))
            except:
                continue
        return HTTPValueWithOptions(value, options)

    def __str__(self) -> str:
        return f"{self.value}{functools.reduce(lambda acc, opt: acc + f'; {opt[0]}={opt[1]}', self.options, '')}"

    def get_option(self, key: str) -> Optional[str]:
        for opt_key, opt_value in self.options:
            if opt_key == key:
                return opt_value

class HTTPContentType(HTTPValueWithOptions):
    @classmethod
    def parse(cls, header_value: str) -> HTTPContentType:
        instance = super(HTTPContentType, cls).parse(header_value)
        instance.__class__ = HTTPContentType
        return instance

    @property
    def mimeType(self) -> str:
        return self.value

    @property
    def charset(self) -> Optional[str]:
        return self.get_option("charset")
    
    @property
    def boundary(self) -> Optional[str]:
        return self.get_option("boundary")

class HTTPContentDisposition(HTTPValueWithOptions):
    @classmethod
    def parse(cls, header_value: str) -> HTTPContentDisposition:
        instance = super(HTTPContentDisposition, cls).parse(header_value)
        instance.__class__ = HTTPContentDisposition
        return instance
    
    @property
    def name(self) -> Optional[str]:
        return self.get_option("name")
    
    @property
    def filename(self) -> Optional[str]:
        return self.get_option("filename")

class HTTPHeaders(list):
    def __str__(self) -> str:
        return "".join([f"{item[0]}: {item[1]}\r\n" for item in self]) + "\r\n"

    def get(self, name: str) -> Optional[str]:
        value = None
        for item in self:
            if item[0].lower() == name.lower():
                value = item[1]
        return value
    
    def getall(self, name: str) -> list[str]:
        return [item[1] for item in filter(lambda item: item[0].lower() == name.lower(), self)]
    
    def delete(self, name: str) -> None:
        for i in reversed([i for i, item in enumerate(self) if item[0].lower() == name.lower()]):
            del self[i]
    
    def replace(self, name: str, value: str) -> None:
        self.delete(name)
        self.append((name, value))

    @property
    def content_type(self) -> Optional[HTTPContentType]:
        if value := self.get('Content-Type'):
            return HTTPContentType.parse(value)
    
    @property
    def content_disposition(self) -> Optional[HTTPContentDisposition]:
        if value := self.get('Content-Disposition'):
            return HTTPContentDisposition.parse(value)

    @property
    def accept_encoding(self) -> Optional[list[str]]:
        if value := self.get('Accept-Encoding'):
            return [item.strip() for item in value.split(',')]
        
    @property
    def content_encoding(self) -> Optional[list[str]]:
        if value := self.get('Content-Encoding'):
            return [item.strip() for item in value.split(',')]
    
    @property
    def content_length(self) -> Optional[int]:
        if value := self.get('Content-Length'):
            return int(value)
    
    @content_length.setter
    def content_length(self, value: int) -> None:
        for item in self:
            if item[0] == "Content-Length":
                item[1] = str(value)

    @property
    def transfer_encoding(self) -> Optional[list[str]]:
        if value := self.get('Transfer-Encoding'):
            return [item.strip() for item in value.split(',')]
    
    @property
    def upgrade_websocket(self) -> bool:
        if connection := self.get('Connection'):
            if connection.lower() == "upgrade":
                if upgrade := self.get("Upgrade"):
                    return upgrade.lower() == "websocket"
        return False
    
    def create_HAR_request_cookies(self) -> list[har.HAR_cookie]:
        if cookie_value := self.get('Cookie'):
            cookie_obj = SimpleCookie(cookie_value)
            return [har.HAR_cookie(
                morsel.key,
                morsel.value,
                morsel['path'] or None,
                morsel['domain'] or None,
                morsel['expires'] or None,
                morsel['httponly'] or None,
                morsel['secure'] or None,
                morsel['samesite'] or None,
                None
            ) for morsel in cookie_obj.values()]
        else:
            return []
    
    def create_HAR_response_cookies(self) -> list[har.HAR_cookie]:
        if cookie_values := self.getall('Set-Cookie'):
            cookie_obj = SimpleCookie()
            for cookie_value in cookie_values:
                cookie_obj.load(cookie_value)
            return [har.HAR_cookie(
                morsel.key,
                morsel.value,
                morsel['path'] or None,
                morsel['domain'] or None,
                morsel['expires'] or None,
                morsel['httponly'] or None,
                morsel['secure'] or None,
                morsel['samesite'] or None,
                None
            ) for morsel in cookie_obj.values()]
        else:
            return []

@dataclass
class HTTPMultipartItem:
    headers: HTTPHeaders
    content: bytes

def parse_body(data: bytes, content_type: HTTPContentType) -> Union[dict, list, str, bytes]:
    if content_type.mimeType in ('application/json', 'application/JSON'):
        try:
            decoded_data = data.decode(content_type.charset or 'utf-8')
        except:
            return data
        try:
            return json.loads(decoded_data)
        except:
            return decoded_data
    elif content_type.mimeType == 'application/x-www-form-urlencoded':
        try:
            decoded_data = data.decode(content_type.charset or 'utf-8')
        except:
            return data
        try:
            return parse_qsl(decoded_data, keep_blank_values=True)
        except:
            return decoded_data
    elif content_type.mimeType == 'multipart/form-data':
        boundary = f"--{content_type.boundary}\r\n".encode()
        end_boundary = f"--{content_type.boundary}--\r\n".encode()
        ended = False
        items = []
        f = BytesIO(data)
        f.readline()
        while not ended:
            headers = HTTPHeaders()
            while (header_line := f.readline().decode().rstrip("\r\n")) != "":
                headers.append(tuple(item.strip() for item in header_line.split(':', 1)))
            content = b""
            while True:
                new_line = f.readline()
                if new_line == boundary:
                    items.append(HTTPMultipartItem(headers, content[:-2]))
                    break
                elif new_line == end_boundary:
                    items.append(HTTPMultipartItem(headers, content[:-2]))
                    ended = True
                    break
                else:
                    content += new_line
        return items
    else:
        try:
            return data.decode(content_type.charset or 'utf-8')
        except:
            return data

def serialize_body(body: Union[dict, list, str, bytes], content_type: HTTPContentType) -> bytes:
    if content_type.mimeType in ('application/json', 'application/JSON'):
        if isinstance(body, (dict, list)):
            return json.dumps(body, ensure_ascii=False).encode(content_type.charset or 'utf-8')
        elif type(body) is str:
            return body.encode(content_type.charset or 'utf-8')
        elif type(body) is bytes:
            return body
    elif content_type.mimeType == 'application/x-www-form-urlencoded':
        if type(body) is list:
            return urlencode(body).encode(content_type.charset or 'utf-8')
        elif type(body) is str:
            return body.encode(content_type.charset or 'utf-8')
        elif type(body) is bytes:
            return body
    elif content_type.mimeType == 'multipart/form-data':
        boundary = f"--{content_type.boundary}\r\n".encode()
        end_boundary = f"--{content_type.boundary}--\r\n".encode()
        content = boundary
        for i, item in enumerate(body):
            content += str(item.headers).encode()
            content += item.content
            content += b"\r\n"
            content += end_boundary if i == len(body) - 1 else boundary
        return content
    else:
        if type(body) is str:
            return body.encode(content_type.charset or 'utf-8')
        elif type(body) is bytes:
            return body

def stringify_body(body: Optional[Union[dict, list, str, bytes]], content_type: Optional[HTTPContentType]) -> str:
    if type(body) is dict or type(body) is list:
        if content_type.mimeType in ('application/json', 'application/JSON'):
            return json.dumps(body, ensure_ascii=False)
        elif content_type.mimeType == 'application/x-www-form-urlencoded':
            return urlencode(body)
        elif content_type.mimeType == 'multipart/form-data':
            boundary = f"--{content_type.boundary}\r\n"
            end_boundary = f"--{content_type.boundary}--\r\n"
            content = boundary
            for i, item in enumerate(body):
                content += str(item.headers)
                try:
                    content += item.content.decode()
                except:
                    content += base64.b64encode(content).decode()
                content += end_boundary if i == len(body) - 1 else boundary
            return content
    elif type(body) is str:
        return body
    elif type(body) is bytes:
        return base64.b64encode(body).decode()
    elif body is None:
        return ""
    else:
        raise TypeError("This body type is not supported.")

class HTTPBody:
    def __init__(self, data: Optional[bytes] = None, content_encoding: Optional[list[str]] = None, content_type: Optional[HTTPContentType] = None) -> None:
        if data:
            if content_encoding:
                data, content_encoding = decompress_with_codings(data, content_encoding)

            if content_type:
                data = parse_body(data, content_type)
        
        self.content_encoding = content_encoding
        self.content_type = content_type
        self.value = data
    
    def __str__(self) -> str:
        return self.stringified
    
    def __getitem__(self, key):
        return self.value[key]
    
    def __setitem__(self, key, value):
        self.value[key] = value
        return self
    
    def __delitem__(self, key):
        del self.value[key]
        return self
    
    @property
    def type(self):
        return type(self.value)
    
    @property
    def stringified(self) -> str:
        return stringify_body(self.value, self.content_type)
    
    @property
    def serialized(self) -> bytes:
        data = self.value

        if self.content_type:
            data = serialize_body(data, self.content_type)
        
        if self.content_encoding:
            data = compress_with_codings(data, self.content_encoding)
        
        return data
    
    def create_HAR_request_postData(self) -> Optional[har.HAR_postData]:
        if not self.value: return None
        if self.content_type:
            mimeType = self.content_type.mimeType
            if mimeType == "application/x-www-form-urlencoded" and self.type is list:
                params = [har.HAR_postData_param(item[0], item[1], None, None, None, None) for item in self.value]
                text = ""
                _encoding = None
            elif mimeType == "multipart/form-data" and self.type is list:
                params = []
                for item in self.value:
                    content_disposition = item.headers.content_disposition
                    content_type = item.headers.content_type
                    name = content_disposition.name if content_disposition else ""
                    fileName = content_disposition.filename if content_disposition else None
                    contentType = str(content_type) if content_type else None
                    _encoding = None
                    try:
                        value = item.content.decode((content_type.charset or 'utf-8') if content_type else 'utf-8')
                    except:
                        value = base64.b64encode(item.content).decode()
                        _encoding = "base64"
                    params.append(har.HAR_postData_param(name, value, fileName, contentType, _encoding, None))
                text = ""
                _encoding = None
            else:
                params = []
                text = self.stringified
                _encoding = "base64" if self.type is bytes else None
        else:
            mimeType = ""
            params = []
            text = self.stringified
            _encoding = "base64" if self.type is bytes else None
        return har.HAR_postData(mimeType, params, text, _encoding, None)
    
    def create_HAR_content(self) -> har.HAR_content:
        size = len(self.serialized) if self.value else 0
        compression = None
        if self.content_type:
            mimeType = self.content_type.mimeType or ""
        else:
            mimeType = ""
        text = self.stringified if self.value else None
        encoding = "base64" if self.type is bytes else None
        return har.HAR_content(size, compression, mimeType, text, encoding, None)
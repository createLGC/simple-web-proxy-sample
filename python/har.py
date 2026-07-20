from dataclasses import dataclass
from typing import Optional

# 仕様: https://w3c.github.io/web-performance/specs/HAR/Overview.html

@dataclass
class HAR_timings:
    blocked: Optional[float]
    dns: Optional[float]
    connect: Optional[float]
    send: float
    wait: float
    receive: float
    ssl: Optional[float]
    comment: Optional[str]

@dataclass
class HAR_cache_item:
    expires: Optional[str]
    lastAccess: str
    eTag: str
    hitCount: int
    comment: Optional[str]

@dataclass
class HAR_cache:
    beforeRequest: Optional[HAR_cache_item]
    afterRequest: Optional[HAR_cache_item]
    comment: Optional[str]

@dataclass
class HAR_content:
    size: int
    compression: Optional[int]
    mimeType: str
    text: Optional[str]
    encoding: Optional[str]
    comment: Optional[str]

@dataclass
class HAR_postData_param:
    name: str
    value: Optional[str]
    fileName: Optional[str]
    contentType: Optional[str]
    _encoding: Optional[str]
    comment: Optional[str]

@dataclass
class HAR_postData:
    mimeType: str
    params: list[HAR_postData_param]
    text: str
    _encoding: Optional[str]
    comment: Optional[str]

@dataclass
class HAR_queryString_item:
    name: str
    value: str
    comment: Optional[str]

@dataclass
class HAR_header:
    name: str
    value: str
    comment: Optional[str]

@dataclass
class HAR_cookie:
    name: str
    value: str
    path: Optional[str]
    domain: Optional[str]
    expires: Optional[str]
    httpOnly: Optional[bool]
    secure: Optional[bool]
    sameSite: Optional[str]
    comment: Optional[str]

@dataclass
class HAR_response:
    status: int
    statusText: str
    httpVersion: str
    cookies: list[HAR_cookie]
    headers: list[HAR_header]
    content: HAR_content
    redirectURL: str
    headersSize: int
    bodySize: int
    comment: Optional[str]

@dataclass
class HAR_request:
    method: str
    url: str
    httpVersion: str
    cookies: list[HAR_cookie]
    headers: list[HAR_header]
    queryString: list[HAR_queryString_item]
    postData: Optional[HAR_postData]
    headersSize: int
    bodySize: int
    comment: Optional[str]

@dataclass
class HAR_entry:
    pageref: Optional[str]
    startedDateTime: str
    time: float
    request: HAR_request
    response: HAR_response
    cache: HAR_cache
    timings: HAR_timings
    _clientIPAddress: Optional[str]
    serverIPAddress: Optional[str]
    connection: Optional[str]
    comment: Optional[str]

@dataclass
class HAR_pageTimings:
    onContentLoad: Optional[str]
    onLoad: Optional[str]
    comment: Optional[str]

@dataclass
class HAR_page:
    startedDateTime: str
    id: str
    title: str
    pageTimings: HAR_pageTimings
    comment: Optional[str]

@dataclass
class HAR_browser:
    name: str
    version: str
    comment: Optional[str]

@dataclass
class HAR_creator:
    name: str
    version: str
    comment: Optional[str]

@dataclass
class HAR_log:
    version: str
    creator: HAR_creator
    browser: Optional[HAR_browser]
    pages: Optional[list[HAR_page]]
    entries: list[HAR_entry]
    comment: Optional[str]

@dataclass
class HAR:
    log: HAR_log

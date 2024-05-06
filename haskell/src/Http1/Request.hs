module Http1.Request (Request(..), parseRequest, sendRequest, printRequest) where

import Http1.Common
import System.IO (Handle)
import qualified Data.ByteString.Char8 as DBC

data Request = Request {
    method :: DBC.ByteString,
    url :: DBC.ByteString,
    version :: DBC.ByteString,
    headers :: [(DBC.ByteString, DBC.ByteString)],
    body :: DBC.ByteString
} deriving Show

parseRequest :: Handle -> IO Request
parseRequest h = do
    (_method, _url, _version) <- parseFirstLine h
    _headers <- parseHeaders h
    _body <- parseBody h _headers
    return Request { method = _method,  url = _url, version = _version, headers = _headers, body = _body }

sendRequest :: Handle -> Request -> IO ()
sendRequest h req = do
    DBC.hPut h $ joinRequestLine req
    DBC.hPut h $ joinHeaders $ headers req
    DBC.hPut h $ DBC.pack "\r\n"
    DBC.hPut h $ body req

printRequest :: Request -> IO ()
printRequest req = do
    DBC.putStr $ joinRequestLine req
    DBC.putStr $ joinHeaders $ headers req
    DBC.putStr $ DBC.pack "\r\n"
    DBC.putStr $ body req

joinRequestLine :: Request -> DBC.ByteString
joinRequestLine req = DBC.concat [DBC.unwords [method req, url req, version req], DBC.pack "\r\n"]
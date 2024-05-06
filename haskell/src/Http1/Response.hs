module Http1.Response (Response(..), parseResponse, sendResponse, printResponse) where

import Http1.Common
import System.IO (Handle)
import qualified Data.ByteString.Char8 as DBC

data Response = Response {
    version :: DBC.ByteString,
    statusCode :: DBC.ByteString,
    statusText :: DBC.ByteString,
    headers :: [(DBC.ByteString, DBC.ByteString)],
    body :: DBC.ByteString
} deriving Show

parseResponse :: Handle -> IO Response
parseResponse h = do
    (_version, _statusCode, _statusText) <- parseFirstLine h
    _headers <- parseHeaders h
    _body <- parseBody h _headers
    return Response { version = _version, statusCode = _statusCode, statusText = _statusText, headers = _headers, body = _body }

sendResponse :: Handle -> Response -> IO ()
sendResponse h res = do
    DBC.hPut h $ joinResponseLine res
    DBC.hPut h $ joinHeaders $ headers res
    DBC.hPut h $ DBC.pack "\r\n"
    DBC.hPut h $ body res

printResponse :: Response -> IO ()
printResponse res = do
    DBC.putStr $ joinResponseLine res
    DBC.putStr $ joinHeaders $ headers res
    DBC.putStr $ DBC.pack "\r\n"
    DBC.putStr $ body res

joinResponseLine :: Response -> DBC.ByteString
joinResponseLine res = DBC.concat [DBC.unwords [version res, statusCode res, statusText res], DBC.pack "\r\n"]
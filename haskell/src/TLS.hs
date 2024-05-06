module TLS (Record, parseRecord, sendRecord) where

import Prelude hiding (length)
import System.IO (Handle)
import Data.ByteString.Lazy.Char8 (ByteString, hGet, hPut, pack)
import Data.Binary.Get (getInt16be, runGet)
import Data.Binary.Put (putInt16be, runPut)

data ContentType = Invalid | ChangeCipherSpec | Alert | Handshake | ApplicationData deriving (Show, Eq)
data ProtocolVersion = V1_0 | V1_1 | V1_2 | V1_3 deriving (Show, Eq)

data Record = Record {
    contentType :: ContentType,
    protocolVersion :: ProtocolVersion,
    length :: Int,
    fragment :: ByteString
} deriving Show

parseRecord :: Handle -> IO Record
parseRecord h = do
    _contentType <- parseContentType h
    _protocolVersion <- parseProtocolVersion h
    _length <- parseLength h
    _fragment <- hGet h _length
    return Record {
        contentType = _contentType,
        protocolVersion = _protocolVersion,
        length = _length,
        fragment = _fragment
    }

parseContentType :: Handle -> IO ContentType
parseContentType h = getContentType <$> hGet h 1
    where
        getContentType byte
            | byte == pack "\x0" = Invalid
            | byte == pack "\x14" = ChangeCipherSpec
            | byte == pack "\x15" = Alert
            | byte == pack "\x16" = Handshake
            | byte == pack "\x17" = ApplicationData
            | otherwise = error "failed to parse TLS ContentType"

parseProtocolVersion :: Handle -> IO ProtocolVersion
parseProtocolVersion h = getProtocolVersion <$> hGet h 2
    where
        getProtocolVersion bytes
            | bytes == pack "\x03\x01" = V1_0
            | bytes == pack "\x03\x02" = V1_1
            | bytes == pack "\x03\x03" = V1_2
            | bytes == pack "\x03\x04" = V1_3
            | otherwise = error "failed to parse TLS ProtocolVersion"

parseLength :: Handle -> IO Int
parseLength h = ntohs <$> hGet h 2
    where ntohs = runGet $ fromIntegral <$> getInt16be

sendRecord :: Handle -> Record -> IO ()
sendRecord h record = do
    sendContentType h $ contentType record
    sendProtocolVersion h $ protocolVersion record
    sendLength h $ length record
    hPut h $ fragment record

sendContentType :: Handle -> ContentType -> IO ()
sendContentType h _contentType = case _contentType of
    Invalid -> hPut h $ pack "\x0"
    ChangeCipherSpec -> hPut h $ pack "\x14"
    Alert -> hPut h $ pack "\x15"
    Handshake -> hPut h $ pack "\x16"
    ApplicationData -> hPut h $ pack "\x17"

sendProtocolVersion :: Handle -> ProtocolVersion -> IO ()
sendProtocolVersion h _protocolVersion = case _protocolVersion of
    V1_0 -> hPut h $ pack "\x03\x01"
    V1_1 -> hPut h $ pack "\x03\x02"
    V1_2 -> hPut h $ pack "\x03\x03"
    V1_3 -> hPut h $ pack "\x03\x04"

sendLength :: Handle -> Int -> IO ()
sendLength h _length = hPut h $ htons _length
    where htons n = runPut $ putInt16be $ fromIntegral n
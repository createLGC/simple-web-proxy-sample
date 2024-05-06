module Http1.Common (
    parseFirstLine,
    parseHeaders,
    parseBody,
    joinHeaders
) where

import System.IO (Handle)
import Numeric (readHex)
import Data.Char (isSpace, toLower)
import qualified Data.ByteString.Char8 as DBC
import qualified Data.Bifunctor

readLine :: Handle -> IO DBC.ByteString
readLine h = DBC.dropWhileEnd isSpace <$> DBC.hGetLine h

parseFirstLine :: Handle -> IO (DBC.ByteString, DBC.ByteString, DBC.ByteString)
parseFirstLine h = do
    elements <- DBC.words <$> readLine h
    return (elements !! 0, elements !! 1, elements !! 2)

parseHeaders :: Handle -> IO [(DBC.ByteString, DBC.ByteString)]
parseHeaders h = do
    result <- parseHeader <$> readLine h
    case result of
        Nothing -> return []
        Just header -> (:) header <$> parseHeaders h

parseHeader :: DBC.ByteString -> Maybe (DBC.ByteString, DBC.ByteString)
parseHeader s
    | s == DBC.empty = Nothing
    | otherwise = Just (key, value)
        where
            key = DBC.strip l
            value = DBC.strip $ DBC.tail r
            (l, r) = DBC.break (==':') s

lookupHeaders :: String -> [(DBC.ByteString, DBC.ByteString)] -> Maybe DBC.ByteString
lookupHeaders key headers = lookup (DBC.pack key) $ map (Data.Bifunctor.first (DBC.map toLower)) headers

parseBody :: Handle -> [(DBC.ByteString, DBC.ByteString)] -> IO DBC.ByteString
parseBody h headers = case lookupHeaders "transfer-encoding" headers of
    Nothing -> case lookupHeaders "content-length" headers of
        Nothing -> return DBC.empty
        Just contentLengthStr -> case DBC.readInt contentLengthStr of
            Nothing -> return DBC.empty
            Just (contentLength, _) -> DBC.hGet h contentLength
    Just _ -> parseTransferEncoding h

parseTransferEncoding :: Handle -> IO DBC.ByteString
parseTransferEncoding h = parseChunk h DBC.empty

parseChunk :: Handle -> DBC.ByteString -> IO DBC.ByteString
parseChunk h acc = do
    chunkHeader <- DBC.hGetLine h
    let chunkSize = fst $ head $ readHex $ DBC.unpack chunkHeader
    if chunkSize == 0
        then parseLastChunk h $ DBC.concat [acc, chunkHeader, DBC.pack "\n"]
        else do
            chunkData <- DBC.hGet h chunkSize
            endLine <- DBC.hGetLine h
            parseChunk h $ DBC.concat [acc, chunkHeader, DBC.pack "\n", chunkData, endLine, DBC.pack "\n"]

parseLastChunk :: Handle -> DBC.ByteString -> IO DBC.ByteString
parseLastChunk h acc = do
    line <- DBC.hGetLine h
    let nextAcc = DBC.concat [acc, line, DBC.pack "\n"]
    if line == DBC.pack "\r"
        then return nextAcc
        else parseLastChunk h nextAcc

joinHeaders :: [(DBC.ByteString, DBC.ByteString)] -> DBC.ByteString
joinHeaders headers = DBC.concat $ map joinHeader headers

joinHeader :: (DBC.ByteString, DBC.ByteString) -> DBC.ByteString
joinHeader header = DBC.concat [fst header, DBC.pack ": ", snd header, DBC.pack "\r\n"]
module Main (main) where

import qualified Http1.Request as H1Req
import qualified Http1.Response as H1Res
import qualified TLS
import qualified URL
import System.Environment (getArgs)
import Data.Char (toLower)
import Network.Socket
import System.IO (IOMode(..), Handle, hClose)
import qualified Data.ByteString.Char8 as DBC
import Control.Concurrent
import Control.Monad
import Control.Exception

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> startServer 8080
        1 -> do
            startServer $ read $ head args
            `catch` (\(SomeException _) -> putStrLn "An invalid port number. Please pass an integer for the port number.")
        _ -> putStrLn "Too many arguments. This program accepts only 1 argument. That is the port number which this server listens."

startServer :: PortNumber -> IO ()
startServer port = do
    server_sock <- socket AF_INET Stream defaultProtocol
    bind server_sock $ SockAddrInet port $ tupleToHostAddress (0, 0, 0, 0)
    listen server_sock 1024
    putStrLn ("proxy server start at " ++ show port ++ "\n")
    forever $ do
        (client_sock, client_addr) <- accept server_sock
        forkIO $ startRelay client_sock

connectToServer :: HostName -> ServiceName -> IO Handle
connectToServer host port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrFamily = AF_INET, addrSocketType = Stream }
    info <- head <$> getAddrInfo (Just hints) (Just host) (Just port)
    server_sock <- openSocket info
    let server_addr = addrAddress info
    connect server_sock server_addr
    socketToHandle server_sock ReadWriteMode

handshake :: Handle -> IO (Either String (H1Req.Request, Handle))
handshake client = do
    request <- H1Req.parseRequest client
    let _url = H1Req.url request
    let url = URL.parseURL $ H1Req.url request
    let host = DBC.unpack $ URL.host url
    let _port = URL.port url
    let port = if DBC.null _port then "80" else DBC.unpack _port
    server <- connectToServer host port
    return $ Right (request, server)
    `catch` (\(SomeException e) -> do
        let body = DBC.pack ("<html><body><p>" ++ show e ++ "</p></body></html>")
        H1Res.sendResponse client H1Res.Response {
            H1Res.version = DBC.pack "HTTP/1.1",
            H1Res.statusCode = DBC.pack "400",
            H1Res.statusText = DBC.pack "Bad Request",
            H1Res.headers = [
                (DBC.pack "Content-Type", DBC.pack "text/html"),
                (DBC.pack "Content-Length", DBC.pack $ show $ DBC.length body),
                (DBC.pack "Proxy-Connection", DBC.pack "close")
            ],
            H1Res.body = body
        }
        hClose client
        return $ Left $ show e
    )

startRelay :: Socket -> IO ()
startRelay client_sock = do
    client <- socketToHandle client_sock ReadWriteMode
    handshakeResult <- handshake client
    case handshakeResult of
        Left errString -> putStrLn ("ERROR " ++ errString)
        Right (request, server) -> do
            let url = H1Req.url request
            putStrLn $ "OPEN  " ++ DBC.unpack url
            if H1Req.method request == DBC.pack "CONNECT"
                then do
                    H1Res.sendResponse client H1Res.Response {
                        H1Res.version = DBC.pack "HTTP/1.1",
                        H1Res.statusCode = DBC.pack "200",
                        H1Res.statusText = DBC.pack "Connection established",
                        H1Res.headers = [],
                        H1Res.body = DBC.empty
                    }
                    _ <- forkFinally (forever $ tlsRelay client server) (\result -> do
                        case result of
                            Left e -> putStrLn $ "CLOSE " ++ DBC.unpack url ++ " " ++ show e
                            Right _ -> return ()
                        hClose client
                        hClose server)
                    _ <- forkFinally (forever $ tlsRelay server client) (\_ -> do
                        hClose server
                        hClose client)
                    return ()
                    `catch` (\(SomeException e) -> do
                        putStrLn $ "CLOSE " ++ DBC.unpack url ++ " " ++ show e
                        hClose client
                        hClose server
                    )
                else do
                    _ <- forkFinally
                        (do
                        httpRelay client server $ Just request
                        forever $ httpRelay client server Nothing)
                        (\result -> do
                        case result of
                            Left e -> putStrLn $ "CLOSE " ++ DBC.unpack url ++ " " ++ show e
                            Right _ -> return ()
                        hClose client
                        hClose server)
                    return ()

tlsRelay :: Handle -> Handle -> IO ()
tlsRelay src dst = TLS.sendRecord dst =<< TLS.parseRecord src

httpRelay :: Handle -> Handle -> Maybe H1Req.Request -> IO ()
httpRelay client server _request = do
    request <- case _request of
        Nothing -> H1Req.parseRequest client
        Just request -> return $ unproxify request
    putStr "\n************************************************ HTTP1Request start\n\n"
    H1Req.printRequest request
    putStr "\n************************************************ HTTP1Request end\n\n"
    H1Req.sendRequest server request
    response <- H1Res.parseResponse server
    putStr "\n************************************************ HTTP1Response start\n\n"
    H1Res.printResponse response
    putStr "\n************************************************ HTTP1Response end\n\n"
    H1Res.sendResponse client response

unproxify :: H1Req.Request -> H1Req.Request
unproxify request = request {
    H1Req.url = URL.stringifyURL url { URL.scheme = DBC.empty, URL.host = DBC.empty },
    H1Req.headers = filter isNotProxyHeader $ H1Req.headers request
}
    where url = URL.parseURL $ H1Req.url request

isNotProxyHeader :: (DBC.ByteString, DBC.ByteString) -> Bool
isNotProxyHeader header = not $ DBC.isPrefixOf (DBC.pack "proxy") $ DBC.map toLower (fst header)
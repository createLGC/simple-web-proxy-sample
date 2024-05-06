module URL(URL(..), parseURL, stringifyURL) where

import Data.ByteString.Char8 as DBC

data URL = URL {
    scheme :: DBC.ByteString,
    user :: DBC.ByteString,
    password :: DBC.ByteString,
    host :: DBC.ByteString,
    port :: DBC.ByteString,
    path :: [DBC.ByteString],
    query :: [(DBC.ByteString, DBC.ByteString)],
    anchor :: DBC.ByteString
} deriving Show

parseURL :: DBC.ByteString -> URL
parseURL urlString = URL { scheme = _scheme, user = _user, password = _password, host = _host, port = _port, path = _path, query = _query, anchor = _anchor }
    where
        (_query, _anchor) = parseQuery withoutPath
        (_path, withoutPath) = parsePath withoutHostAndPort
        (_host, _port, withoutHostAndPort) = parseHostAndPort withoutUserAndPassword
        (_user, _password, withoutUserAndPassword) = parseUserAndPassword withoutScheme
        (_scheme, withoutScheme) = parseScheme urlString

parseScheme :: DBC.ByteString -> (DBC.ByteString, DBC.ByteString)
parseScheme urlString = if DBC.isInfixOf (DBC.pack "//") urlString
    then
        let (_scheme, rest) = DBC.breakSubstring (DBC.pack "//") urlString
        in (DBC.dropEnd 1 _scheme, DBC.drop 2 rest)
    else (DBC.empty, urlString)

parseUserAndPassword :: DBC.ByteString -> (DBC.ByteString, DBC.ByteString, DBC.ByteString)
parseUserAndPassword urlString = if DBC.elem '@' urlString
    then
        let (userAndPassWord, rest) = DBC.break (=='@') urlString
            (_user, _password) = DBC.break (==':') userAndPassWord
        in (_user, DBC.drop 1 _password, DBC.drop 1 rest)
    else (DBC.empty, DBC.empty, urlString)

parseHostAndPort :: DBC.ByteString -> (DBC.ByteString, DBC.ByteString, DBC.ByteString)
parseHostAndPort urlString = (_host, DBC.drop 1 _port, DBC.drop 1 rest)
    where
        (_host, _port) = DBC.break (==':') hostAndPort
        (hostAndPort, rest) = DBC.break (=='/') urlString

parsePath :: DBC.ByteString -> ([DBC.ByteString], DBC.ByteString)
parsePath urlString = (pathComponents, DBC.drop 1 rest)
    where
        pathComponents = Prelude.filter (/=DBC.empty) $ DBC.split '/' _path
        (_path, rest) = DBC.break (=='?') urlString

parseQuery :: DBC.ByteString -> ([(DBC.ByteString, DBC.ByteString)], DBC.ByteString)
parseQuery urlString = (queries, DBC.drop 1 rest)
    where
        queries = Prelude.map splitQuery $ Prelude.filter (/=DBC.empty) $ DBC.split '&' queryString
        splitQuery _query = (k, DBC.drop 1 v) where (k, v) = DBC.break (=='=') _query
        (queryString, rest) = DBC.break (=='#') urlString

stringifyURL :: URL -> DBC.ByteString
stringifyURL url = DBC.concat [
        stringifyScheme url,
        stringifyUserAndPassword url,
        stringifyHostAndPort url,
        stringifyPath url,
        stringifyQuery url,
        stringifyAnchor url
    ]

stringifyScheme :: URL -> DBC.ByteString
stringifyScheme url = if not $ DBC.null _scheme
    then DBC.concat [_scheme, DBC.pack "://"]
    else DBC.empty
    where _scheme = scheme url

stringifyUserAndPassword :: URL -> DBC.ByteString
stringifyUserAndPassword url = if not $ DBC.null _user
    then if not $ DBC.null _password
        then DBC.concat [_user, DBC.pack ":", _password, DBC.pack "@"]
        else DBC.concat [_user, DBC.pack "@"]
    else DBC.empty
    where
        _user = user url
        _password = password url

stringifyHostAndPort :: URL -> DBC.ByteString
stringifyHostAndPort url = if not $ DBC.null _host
    then if not $ DBC.null _port
        then DBC.concat [_host, DBC.pack ":", _port]
        else _host
    else DBC.empty
    where
        _host = host url
        _port = port url

stringifyPath :: URL -> DBC.ByteString
stringifyPath url = if Prelude.null _path
    then if (Prelude.null (query url)) && (DBC.null (scheme url)) && (not (DBC.null (host url)))
        then DBC.empty -- CONNECTの時
        else DBC.pack "/"
    else DBC.concat [DBC.pack "/", DBC.intercalate (DBC.pack "/") _path]
    where _path = path url

stringifyQuery :: URL -> DBC.ByteString
stringifyQuery url = if not $ Prelude.null _query
    then DBC.concat [DBC.pack "?", DBC.intercalate (DBC.pack "&") $ Prelude.map (\(k, v) -> DBC.concat [k, DBC.pack "=", v]) _query]
    else DBC.empty
    where _query = query url

stringifyAnchor :: URL -> DBC.ByteString
stringifyAnchor url = if not $ DBC.null _anchor
    then DBC.concat [DBC.pack "#", _anchor]
    else DBC.empty
    where _anchor = anchor url
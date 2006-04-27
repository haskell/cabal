-- Reads a method call in XML from standard input, sends it to a
-- server and prints the response to standard output. Must be editied 
-- to use the right server URL.

import System (getArgs, exitFailure)
import IO (hPutStrLn, stderr)
import Data.Char
import Network.URI

import Network.XmlRpc.Internals
import Network.HTTP

parseArgs :: IO String
parseArgs = do
	    args <- getArgs
	    case args of 
		      [url] -> return url
		      _ -> do
			   hPutStrLn stderr "Usage: raw_call url"
			   exitFailure

main = do
       url <- parseArgs
       c <- getContents
       post url c
       return ()



userAgent :: String
userAgent = "Haskell XmlRpcClient/0.1"

-- | Handle connection errors.
handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

post :: String -> String -> IO String
post url content = 
    case parseURI url of
		      Nothing -> fail ("Bad uri: '" ++ url ++ "'")
		      Just uri -> post_ uri content

post_ :: URI -> String -> IO String
post_ uri content = 
    do
    putStrLn "-- Begin request --"
    putStrLn (show (request uri content))
    putStrLn content
    putStrLn "-- End request --"
    eresp <- simpleHTTP (request uri content)
    resp <- handleE (fail . show) eresp
    case rspCode resp of
		      (2,0,0) -> do
				 putStrLn "-- Begin response --"
				 putStrLn (show resp)
				 putStrLn (rspBody resp)
				 putStrLn "-- End response --"
				 return (rspBody resp)
		      _ -> fail (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

-- | Create an XML-RPC compliant HTTP request
request :: URI -> String -> Request
request uri content = Request{ rqURI = uri, 
		       rqMethod = POST, 
		       rqHeaders = headers, 
		       rqBody = content }
    where
    -- the HTTP module adds a Host header based on the URI
    headers = [Header HdrUserAgent userAgent,
	       Header HdrContentType "text/xml",
	       Header HdrContentLength (show (length content))]

-- A simple test program which takes a url on the commandline
-- and outputs the contents to stdout.

-- ghc --make -package HTTP get.hs -o get

import Data.Char (intToDigit)
import Network.HTTP
import Network.URI
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main = 
    do
    args <- getArgs
    case args of 
	[addr] -> case parseURI addr of
		       Nothing -> err "Could not parse URI"
		       Just uri -> do
				   cont <- get uri
			           putStr cont
	_ -> err "Usage: get <url>"

err :: String -> IO a
err msg = do 
	  hPutStrLn stderr msg
	  exitFailure

get :: URI -> IO String
get uri =
    do
    eresp <- simpleHTTP (request uri)
    resp <- handleE (err . show) eresp
    case rspCode resp of
                      (2,0,0) -> return (rspBody resp)
                      _ -> err (httpError resp)
    where
    showRspCode (a,b,c) = map intToDigit [a,b,c]
    httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp

request :: URI -> Request
request uri = Request{ rqURI = uri,
                       rqMethod = GET,
                       rqHeaders = [],
                       rqBody = "" }

handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v

{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists 
-----------------------------------------------------------------------------
module Hackage.HttpUtils (getHTTP, proxy) where

import Network.HTTP (Request (..), Response (..), RequestMethod (..), Header(..), HeaderName(..))
import Network.URI (URI (..), URIAuth (..), parseURI)
import Network.Stream (Result)
import Network.Browser (Proxy (..), Authority (..), browse, setProxy, request)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (mplus)
import Control.Exception (try)
#ifdef WIN32
import System.Win32.Registry (hKEY_CURRENT_USER, regOpenKey, regQueryValue, regCloseKey)
import Control.Exception (bracket)
#endif
import System.Environment (getEnvironment)

-- try to read the system proxy settings on windows or unix
proxyString :: IO String
#ifdef WIN32
-- read proxy settings from the windows registry
proxyString = bracket (regOpenKey hive path) regCloseKey
	     (flip regQueryValue (Just "ProxyServer"))
  where
    -- some sources say proxy settings should be at 
    -- HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows
    --                   \CurrentVersion\Internet Settings\ProxyServer
    -- but if the user sets them with IE connection panel they seem to
    -- end up in the following place:
    hive  = hKEY_CURRENT_USER
    path = "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"
#else
-- read proxy settings by looking for an env var
proxyString = do
  env <- getEnvironment
  return (fromMaybe "" $ lookup "http_proxy" env
                 `mplus` lookup "HTTP_PROXY" env)
#endif

-- |Get the local proxy settings  
proxy :: IO Proxy
proxy = try proxyString >>= return . maybe NoProxy uri2proxy
                                   . either (const Nothing) parseURI

mkRequest :: URI -> Request
mkRequest uri = Request{ rqURI     = uri
                       , rqMethod  = GET
                       , rqHeaders = [Header HdrUserAgent "Cabal"]
                       , rqBody    = "" }

uri2proxy :: URI -> Proxy
uri2proxy uri = Proxy host auth
  where (URIAuth auth' host _) = fromJust $ uriAuthority uri
        auth = if null auth'
                 then Nothing
                 else Just (AuthBasic "" usr pwd uri)
        (usr,pwd') = break (==':') auth'
        pwd        = case pwd' of
                       ':':cs -> cs
                       _      -> pwd'

-- |Carry out a GET request, using the local proxy settings
getHTTP :: URI -> IO (Result Response)
getHTTP uri = do p   <- proxy
                 let req = mkRequest uri
                 (_, resp) <- browse (setProxy p >> request req)
                 return (Right resp)

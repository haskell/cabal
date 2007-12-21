{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists 
-----------------------------------------------------------------------------
module Hackage.HttpUtils (getHTTP, proxy) where

import Network.HTTP (Request (..), Response (..), RequestMethod (..), Header(..), HeaderName(..))
import Network.URI (URI (..), URIAuth (..), parseURI)
import Network.Stream (Result)
import Network.Browser (Proxy (..), Authority (..), browse, setProxy, request)
import Data.Maybe (fromJust)
#ifdef WIN32
import System.Win32.Registry (hKEY_CURRENT_USER, regOpenKey, regQueryValue, regCloseKey)
#else
import System.Posix.Env (getEnv)
#endif

-- try to read the system proxy settings on windows or unix
proxyURI :: IO (Maybe URI)
#ifdef WIN32
-- read proxy settings from the windows registry
proxyURI = do hKey <- return key
              uri  <- regOpenKey hKey path 
                      >>= flip regQueryValue (Just "ProxyServer") 
                      >>= return . parseURI
              regCloseKey hKey
              return uri
    where {-some sources say proxy settings should be at 
            HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows\CurrentVersion\Internet Settings\ProxyServer
            but if the user sets them with IE connection panel they seem to end up in the 
            following place within HKEY_CURRENT_USER. -}
          key  = hKEY_CURRENT_USER
          path = "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"
#else
-- read proxy settings by looking for an env var
proxyURI = getEnv "http_proxy" >>= maybe (getEnv "HTTP_PROXY" 
                                          >>= parseURIM) (parseURIM . Just)
    where parseURIM :: Maybe String -> IO (Maybe URI)
          parseURIM = return . maybe Nothing parseURI
#endif

-- |Get the local proxy settings  
proxy :: IO Proxy
proxy = proxyURI >>= return . uri2proxy

mkRequest :: URI -> IO Request
mkRequest uri = return Request{ rqURI     = uri
                              , rqMethod  = GET
                              , rqHeaders = [Header HdrUserAgent "Cabal"]
                              , rqBody    = "" }

uri2proxy :: Maybe URI -> Proxy
uri2proxy = maybe NoProxy (\uri ->
                           let (URIAuth auth' host _) = fromJust $ uriAuthority uri
                               auth = if null auth' then Nothing
                                      else Just (AuthBasic "" usr pwd uri)
                               (usr,pwd') = break (==':') auth'
                               pwd        = case pwd' of
                                              ':':cs -> cs
                                              _      -> pwd'
                               in
                           Proxy host auth)

-- |Carry out a GET request, using the local proxy settings
getHTTP :: URI -> IO (Result Response)
getHTTP uri = do p   <- proxy
                 req <- mkRequest uri
                 (_, resp) <- browse (setProxy p >> request req)
                 return (Right resp)

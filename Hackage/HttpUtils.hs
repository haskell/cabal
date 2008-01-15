{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists 
-----------------------------------------------------------------------------
module Hackage.HttpUtils (getHTTP, proxy) where

import Network.HTTP (Request (..), Response (..), RequestMethod (..), Header(..), HeaderName(..))
import Network.URI (URI (..), URIAuth (..), parseURI)
import Network.Stream (Result)
import Network.Browser (Proxy (..), Authority (..), browse, setProxy, request)
import Control.Monad (mplus)
#ifdef WIN32
import System.Win32.Registry (hKEY_CURRENT_USER, regOpenKey, regQueryValue, regCloseKey)
import Control.Exception (try, bracket)
#endif
import System.Environment (getEnvironment)

import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (warn)

-- try to read the system proxy settings on windows or unix
proxyString :: IO (Maybe String)
#ifdef WIN32
-- read proxy settings from the windows registry
proxyString = fmap (either (const Nothing) Just) $ try $
                bracket (regOpenKey hive path) regCloseKey
                  (\hkey -> regQueryValue hkey (Just "ProxyServer"))
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
  return (lookup "http_proxy" env `mplus` lookup "HTTP_PROXY" env)
#endif

-- |Get the local proxy settings  
proxy :: Verbosity -> IO Proxy
proxy verbosity = do
  mstr <- proxyString
  case mstr of
    Nothing     -> return NoProxy
    Just str    -> case parseURI str of
      Nothing   -> do warn verbosity $ "invalid proxy uri: " ++ show str
                      return NoProxy
      Just uri  -> case uri2proxy uri of
        Nothing -> do warn verbosity $ "invalid http proxy uri: " ++ show str
                      warn verbosity $ "proxy uri must be http with a hostname"
                      return NoProxy
        Just p  -> return p

uri2proxy :: URI -> Maybe Proxy
uri2proxy uri@URI{ uriScheme = "http:"
                 , uriAuthority = Just (URIAuth auth' host port)
                 } = Just (Proxy (host ++ port) auth)
  where auth = if null auth'
                 then Nothing
                 else Just (AuthBasic "" usr pwd uri)
        (usr,pwd') = break (==':') auth'
        pwd        = case pwd' of
                       ':':cs -> cs
                       _      -> pwd'
uri2proxy _ = Nothing

mkRequest :: URI -> Request
mkRequest uri = Request{ rqURI     = uri
                       , rqMethod  = GET
                       , rqHeaders = [Header HdrUserAgent "Cabal"]
                       , rqBody    = "" }

-- |Carry out a GET request, using the local proxy settings
getHTTP :: Verbosity -> URI -> IO (Result Response)
getHTTP verbosity uri = do
                 p   <- proxy verbosity
                 let req = mkRequest uri
                 (_, resp) <- browse (setProxy p >> request req)
                 return (Right resp)

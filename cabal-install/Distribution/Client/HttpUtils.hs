{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists 
-----------------------------------------------------------------------------
module Distribution.Client.HttpUtils (
    downloadURI,
    getHTTP,
    proxy,
    isOldHackageURI
  ) where

import Network.HTTP
         ( Request (..), Response (..), RequestMethod (..)
         , Header(..), HeaderName(..) )
import Network.URI
         ( URI (..), URIAuth (..), parseAbsoluteURI )
import Network.Stream
         ( Result, ConnError(..) )
import Network.Browser
         ( Proxy (..), Authority (..), browse
         , setOutHandler, setErrHandler, setProxy, request)
import Control.Monad
         ( mplus, join, liftM2 )
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy (ByteString)
#ifdef WIN32
import System.Win32.Types
         ( DWORD, HKEY )
import System.Win32.Registry
         ( hKEY_CURRENT_USER, regOpenKey, regCloseKey
         , regQueryValue, regQueryValueEx )
import Control.Exception
         ( bracket )
import Distribution.Compat.Exception
         ( handleIO )
import Foreign
         ( toBool, Storable(peek, sizeOf), castPtr, alloca )
#endif
import System.Environment (getEnvironment)

import qualified Paths_cabal_install (version)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die, info, warn, debug
         , copyFileVerbose, writeFileAtomic )
import Distribution.Text
         ( display )
import qualified System.FilePath.Posix as FilePath.Posix
         ( splitDirectories )

-- FIXME: all this proxy stuff is far too complicated, especially parsing
-- the proxy strings. Network.Browser should have a way to pick up the
-- proxy settings hiding all this system-dependent stuff below.

-- try to read the system proxy settings on windows or unix
proxyString, envProxyString, registryProxyString :: IO (Maybe String)
#ifdef WIN32
-- read proxy settings from the windows registry
registryProxyString = handleIO (\_ -> return Nothing) $
  bracket (regOpenKey hive path) regCloseKey $ \hkey -> do
    enable <- fmap toBool $ regQueryValueDWORD hkey "ProxyEnable"
    if enable
        then fmap Just $ regQueryValue hkey (Just "ProxyServer")
        else return Nothing
  where
    -- some sources say proxy settings should be at 
    -- HKEY_LOCAL_MACHINE\SOFTWARE\Policies\Microsoft\Windows
    --                   \CurrentVersion\Internet Settings\ProxyServer
    -- but if the user sets them with IE connection panel they seem to
    -- end up in the following place:
    hive  = hKEY_CURRENT_USER
    path = "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings"

    regQueryValueDWORD :: HKEY -> String -> IO DWORD
    regQueryValueDWORD hkey name = alloca $ \ptr -> do
      regQueryValueEx hkey name (castPtr ptr) (sizeOf (undefined :: DWORD))
      peek ptr
#else
registryProxyString = return Nothing
#endif

-- read proxy settings by looking for an env var
envProxyString = do
  env <- getEnvironment
  return (lookup "http_proxy" env `mplus` lookup "HTTP_PROXY" env)

proxyString = liftM2 mplus envProxyString registryProxyString


-- |Get the local proxy settings  
proxy :: Verbosity -> IO Proxy
proxy verbosity = do
  mstr <- proxyString
  case mstr of
    Nothing   -> return NoProxy
    Just str  -> case parseHttpProxy str of
      Nothing -> do
        warn verbosity $ "invalid http proxy uri: " ++ show str
        warn verbosity $ "proxy uri must be http with a hostname"
        warn verbosity $ "ignoring http proxy, trying a direct connection"
        return NoProxy
      Just p  -> return p
--TODO: print info message when we're using a proxy

-- | We need to be able to parse non-URIs like @\"wwwcache.example.com:80\"@
-- which lack the @\"http://\"@ URI scheme. The problem is that
-- @\"wwwcache.example.com:80\"@ is in fact a valid URI but with scheme
-- @\"wwwcache.example.com:\"@, no authority part and a path of @\"80\"@.
--
-- So our strategy is to try parsing as normal uri first and if it lacks the
-- 'uriAuthority' then we try parsing again with a @\"http://\"@ prefix.
--
parseHttpProxy :: String -> Maybe Proxy
parseHttpProxy str = join
                   . fmap uri2proxy
                   $ parseHttpURI str
             `mplus` parseHttpURI ("http://" ++ str)
  where
    parseHttpURI str' = case parseAbsoluteURI str' of
      Just uri@URI { uriAuthority = Just _ }
         -> Just (fixUserInfo uri)
      _  -> Nothing

fixUserInfo :: URI -> URI
fixUserInfo uri = uri{ uriAuthority = f `fmap` uriAuthority uri }
    where
      f a@URIAuth{ uriUserInfo = s } =
          a{ uriUserInfo = case reverse s of
                             '@':s' -> reverse s'
                             _      -> s
           }
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

mkRequest :: URI -> Request ByteString
mkRequest uri = Request{ rqURI     = uri
                       , rqMethod  = GET
                       , rqHeaders = [Header HdrUserAgent userAgent]
                       , rqBody    = ByteString.empty }
  where userAgent = "cabal-install/" ++ display Paths_cabal_install.version

-- |Carry out a GET request, using the local proxy settings
getHTTP :: Verbosity -> URI -> IO (Result (Response ByteString))
getHTTP verbosity uri = do
                 p   <- proxy verbosity
                 let req = mkRequest uri
                 (_, resp) <- browse $ do
                                setErrHandler (warn verbosity . ("http error: "++))
                                setOutHandler (debug verbosity)
                                setProxy p
                                request req
                 return (Right resp)

downloadURI :: Verbosity
            -> URI      -- ^ What to download
            -> FilePath -- ^ Where to put it
            -> IO ()
downloadURI verbosity uri path | uriScheme uri == "file:" =
  copyFileVerbose verbosity (uriPath uri) path
downloadURI verbosity uri path = do
  result <- getHTTP verbosity uri
  let result' = case result of
        Left  err -> Left err
        Right rsp -> case rspCode rsp of
          (2,0,0) -> Right (rspBody rsp)
          (a,b,c) -> Left err
            where
              err = ErrorMisc $ "Unsucessful HTTP code: "
                             ++ concatMap show [a,b,c]

  case result' of
    Left err   -> die $ "Failed to download " ++ show uri ++ " : " ++ show err
    Right body -> do
      info verbosity ("Downloaded to " ++ path)
      writeFileAtomic path (ByteString.unpack body)
      --FIXME: check the content-length header matches the body length.
      --TODO: stream the download into the file rather than buffering the whole
      --      thing in memory.
      --      remember the ETag so we can not re-download if nothing changed.

-- Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri
    = case uriAuthority uri of
        Just (URIAuth {uriRegName = "hackage.haskell.org"}) ->
            FilePath.Posix.splitDirectories (uriPath uri) == ["/","packages","archive"]
        _ -> False

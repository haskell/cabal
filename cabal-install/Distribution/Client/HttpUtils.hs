-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists
-----------------------------------------------------------------------------
module Distribution.Client.HttpUtils (
    DownloadResult(..),
    downloadURI,
    uploadToURI,
    isOldHackageURI
  ) where
{-
import Network.HTTP
         ( Request (..), Response (..), RequestMethod (..)
         , Header(..), HeaderName(..), lookupHeader )
-}
import Network.HTTP.Proxy ( Proxy(..), fetchProxy)

import Network.URI
         ( URI (..), URIAuth (..) )
{-
import Network.Browser
         ( BrowserAction, browse
         , setOutHandler, setErrHandler, setProxy, setAuthorityGen, request)
import Network.Stream
         ( Result, ConnError(..) )
-}
import Control.Applicative
import Control.Monad
         ( when )
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy (ByteString)
import Data.List
         ( isPrefixOf )
import Data.Maybe
         ( fromMaybe, listToMaybe )
import qualified Paths_cabal_install (version)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die, info, warn, debug, notice
         , copyFileVerbose, writeFileAtomic)
import Distribution.System
         ( buildOS, buildArch )
import Distribution.Text
         ( display )
import Data.Char
         ( isSpace )
import qualified System.FilePath.Posix as FilePath.Posix
         ( splitDirectories )
import System.FilePath
         ( (<.>) )
import System.Directory
         ( doesFileExist, renameFile, removeFile )
import System.IO.Error
         ( isDoesNotExistError )
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import Text.Read (readMaybe)
import System.IO (hClose, openTempFile)
import System.FilePath (takeFileName, takeDirectory)
import qualified Control.Exception as Exception

import Distribution.Simple.Program.Run
import Distribution.Simple.Utils
         ( die, rawSystemExit, rawSystemIOWithEnv, rawSystemStdInOut
         , toUTF8, fromUTF8, normaliseLineEndings )
import System.Exit
         ( ExitCode(..), exitWith )

data DownloadResult = FileAlreadyInCache | FileDownloaded FilePath deriving (Eq)

-- Trim
trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace

{-
-- |Get the local proxy settings
--TODO: print info message when we're using a proxy based on verbosity
proxy :: Verbosity -> IO Proxy
proxy _verbosity = do
  p <- fetchProxy True
  -- Handle empty proxy strings
  return $ case p of
    Proxy uri auth ->
      let uri' = trim uri in
      if uri' == "" then NoProxy else Proxy uri' auth
    _ -> p
-}

userAgent :: String
userAgent = concat [ "cabal-install/", display Paths_cabal_install.version
                   , " (", display buildOS, "; ", display buildArch, ")"
                   ]

data HttpTransportType = CurlLoc FilePath | WGetLoc FilePath | PowerShellLoc FilePath | BITSAdminLoc FilePath | HttpTransport -- TODO add HTTPS Transport Here.

-- TODO also check config for paths explicitly?
findHttpTransport :: Verbosity -> IO HttpTransportType
findHttpTransport verbosity =
    let fpl = findProgramLocation verbosity
        orFind :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
        orFind x y = maybe y (return . Just) =<< x
        result =   (fmap CurlLoc <$> fpl "curl")
                  `orFind` (fmap WGetLoc <$> fpl "wget")
                  `orFind` (fmap PowerShellLoc <$> fpl "powershell")
                  `orFind` (fmap BITSAdminLoc <$> fpl "bitsadmin")
    in return . fromMaybe HttpTransport =<< result

getProgramInvocationOutputAndErrors :: Verbosity -> ProgramInvocation -> IO (String, String)
getProgramInvocationOutputAndErrors verbosity
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = envOverrides,
    progInvokeCwd   = mcwd,
    progInvokeInput = minputStr,
    progInvokeOutputEncoding = encoding
  } = do
    let utf8 = case encoding of IOEncodingUTF8 -> True; _ -> False
        decode | utf8      = fromUTF8 . normaliseLineEndings
               | otherwise = id
    menv <- getEffectiveEnvironment envOverrides
    (output, errors, exitCode) <- rawSystemStdInOut verbosity
                                    path args
                                    mcwd menv
                                    input utf8
    when (exitCode /= ExitSuccess) $
      die $ "'" ++ path ++ "' exited with an error:\n" ++ errors ++ "\n" ++ decode output
    return (decode output, errors)
  where
    input =
      case minputStr of
        Nothing       -> Nothing
        Just inputStr -> Just $
          case encoding of
            IOEncodingText -> (inputStr, False)
            IOEncodingUTF8 -> (toUTF8 inputStr, True) -- use binary mode for utf8

takeUntil p [] = []
takeUntil p (x:xs) = if p x then [x] else x : takeUntil p xs

getHTTP :: Verbosity -> URI -> Maybe String -> FilePath -> IO (Maybe String, Int)
getHTTP verbosity uri etag destPath = executeGet =<< findHttpTransport verbosity
    where
      executeGet (CurlLoc fp) =
          let prog = simpleConfiguredProgram "curl" (FoundOnSystem fp)
              args = [show uri,"-o",destPath,"-L","--write-out","%{http_code}","-A",userAgent] ++ maybe [] (\t -> ["--header","If-None-Match: " ++ t]) etag -- TODO set curl verbosity?
              parseResponse x = case readMaybe $ trim x of
                          Just i -> return (Nothing, i) -- TODO extract real etag
                          Nothing -> die $ "Failed to download " ++ show uri ++ " : No Status Code could be parsed from Response: " ++ x
          in parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog args)
      executeGet (WGetLoc fp) =
          let prog = simpleConfiguredProgram "wget" (FoundOnSystem fp)
              args = ["-S",show uri,"--output-document="++destPath,"--user-agent="++userAgent] ++ maybe [] (\t -> ["--header","If-None-Match: " ++ t]) etag -- TODO set verbosity?
              parseResponse x =
                  let resp = reverse . takeUntil ("HTTP/" `isPrefixOf`) . reverse . map (dropWhile isSpace) . lines $ x
                  in case readMaybe =<< listToMaybe . drop 1 . words =<< listToMaybe resp of
                       Just i -> return (Nothing, i) --TODO etags
                       Nothing -> die $ "Failed to download " ++ show uri ++ " : No Status Code could be parsed from Response: " ++ x
          in parseResponse . snd =<< getProgramInvocationOutputAndErrors verbosity (programInvocation prog args)
      -- TODO PowerShell isn't finished
      executeGet (PowerShellLoc fp) = do
          p <- fetchProxy True
          let prog = simpleConfiguredProgram "powershell" (FoundOnSystem fp)
              escape x = '"' : x ++ "\"" --TODO write/find real escape.
              proxySettings _ = [] --TODO extract real settings

              parseResponse x = case readMaybe $ trim x of
                          Just i -> return (Nothing, i) -- TODO extract real etag
                          Nothing -> die $ "Failed to download " ++ show uri ++ " : No Status Code could be parsed from Response: " ++ x

              script = unlines . map (++";") $
                       ["$wc = new-object system.net.webclient",
                        "$wc.Headers.Add(\"user-agent\","++escape userAgent++")"]
                       ++ maybe [] (\t -> ["$wc.Headers.Add(\"If-None-Match " ++ t ++ ")"]) etag
                       ++ proxySettings p
                       ++ ["$wc.DownloadFile("++ escape (show uri) ++ "," ++ escape destPath ++ ")",
                           "$wc.ResponseHeaders.Item(\"status\")"]
          parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog [script])
-- TODO bitsadmin
-- TODO fallback to http

{-
-- |Carry out a GET request, using the local proxy settings
getHTTP :: Verbosity
        -> URI
        -> Maybe String -- ^ Optional etag to check if we already have the latest file.
        -> IO (Result (Response ByteString))
getHTTP verbosity uri etag = liftM (\(_, resp) -> Right resp) $ undefined
--                                   cabalBrowse verbosity (return ()) (request (mkRequest uri etag))


cabalBrowse :: Verbosity
            -> BrowserAction s ()
            -> BrowserAction s a
            -> IO a
cabalBrowse verbosity auth act = do
    p   <- proxy verbosity
    handleJust
        (guard . isDoesNotExistError)
        (const . die $ "Couldn't establish HTTP connection. "
                    ++ "Possible cause: HTTP proxy server is down.") $
        browse $ do
            setProxy p
            setErrHandler (warn verbosity . ("http error: "++))
            setOutHandler (debug verbosity)
            auth
            setAuthorityGen (\_ _ -> return Nothing)
            act
-}


withTempFileName :: FilePath
             -> String
             -> (FilePath -> IO a) -> IO a
withTempFileName tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, _) -> (`when` removeFile name) =<< doesFileExist name)
    (\(name, h) -> hClose h >> action name)

downloadURI :: Verbosity
            -> URI      -- ^ What to download
            -> FilePath -- ^ Where to put it
            -> IO DownloadResult
downloadURI verbosity uri path | uriScheme uri == "file:" = do
  copyFileVerbose verbosity (uriPath uri) path
  return (FileDownloaded path)
  -- Can we store the hash of the file so we can safely return path when the
  -- hash matches to avoid unnecessary computation?

downloadURI verbosity uri path = withTempFileName (takeDirectory path) (takeFileName path) $ \tmpFile -> do
  let etagPath = path <.> "etag"
  targetExists   <- doesFileExist path
  etagPathExists <- doesFileExist etagPath
  -- In rare cases the target file doesn't exist, but the etag does.
  etag <- if targetExists && etagPathExists
            then Just <$> readFile etagPath
            else return Nothing

  result <- getHTTP verbosity uri etag tmpFile

  -- Only write the etag if we get a 200 response code.
  -- A 304 still sends us an etag header.
  case result of
    (Just newEtag, 200) -> writeFile etagPath newEtag
    _ -> return ()

  case snd result of
    200 -> do
        info verbosity ("Downloaded to " ++ path)
        renameFile tmpFile path
        return (FileDownloaded path)
    304 -> do
        notice verbosity "Skipping download: Local and remote files match."
        return FileAlreadyInCache
    errCode ->  die $ "Failed to download " ++ show uri ++ " : HTTP code " ++ show errCode

uploadToURI :: Verbosity -> URI -> FilePath -> Maybe (String,String) -> IO (Int, String)
uploadToURI verbosity uri path auth = executeUpload =<< findHttpTransport verbosity
    where
      executeUpload (CurlLoc fp) =
          let prog = simpleConfiguredProgram "curl" (FoundOnSystem fp)
              args = [show uri,"-F","package=@"++path,"--write-out","%{http_code}","-A",userAgent] ++ maybe [] (\(u,p) -> ["--digest","-u",u++":"++p]) auth -- TODO set curl verbosity
              parseResponse x = case readMaybe . trim =<< listToMaybe . take 1 . reverse . lines =<< return x of
                          Just i -> return (i,x) -- TODO extract error?
                          Nothing -> die $ "Failed to download " ++ show uri ++ " : No Status Code could be parsed from Response: " ++ x
          in parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog args)
-- TODO handle other transports

-- Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri
    = case uriAuthority uri of
        Just (URIAuth {uriRegName = "hackage.haskell.org"}) ->
            FilePath.Posix.splitDirectories (uriPath uri) == ["/","packages","archive"]
        _ -> False

-----------------------------------------------------------------------------
-- | Separate module for HTTP actions, using a proxy server if one exists
-----------------------------------------------------------------------------
module Distribution.Client.HttpUtils (
    DownloadResult(..),
    configureTransport,
    setupTransportDb,
    HttpTransport(..),
    downloadURI,
    uploadToURI,
    isOldHackageURI
  ) where

import Network.HTTP
         ( Request (..), Response (..), RequestMethod (..)
         , Header(..), HeaderName(..), lookupHeader )
import Network.HTTP.Proxy ( Proxy(..), fetchProxy)
import Network.URI
         ( URI (..), URIAuth (..) )
import Network.Browser
         ( browse, setOutHandler, setErrHandler, setProxy, setAuthorityGen, request
         , Authority(..), addAuthority)
import Control.Applicative
import qualified Control.Exception as Exception
import Control.Monad
         ( when, guard )
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.List
         ( isPrefixOf )
import Data.Maybe
         ( listToMaybe )
import qualified Paths_cabal_install (version)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils
         ( die, info, warn, debug, notice, writeFileAtomic
         , copyFileVerbose,  withTempFile
         , rawSystemStdInOut, toUTF8, fromUTF8, normaliseLineEndings )
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
         ( simpleProgram, getProgramInvocationOutput, programInvocation
         , ConfiguredProgram, ProgramInvocation(..) )
import Distribution.Simple.Program.Db
         ( ProgramDb, configureAllKnownPrograms, addKnownPrograms, lookupProgram
         , defaultProgramDb )
import Distribution.Simple.Program.Run
        ( IOEncoding(..), getEffectiveEnvironment )
import Text.Read (readMaybe)
import Numeric (showHex)
import System.IO (hClose, openTempFile)
import System.FilePath (takeFileName, takeDirectory)
import System.Random (randomRIO)
import System.Exit (ExitCode(..))

data DownloadResult = FileAlreadyInCache | FileDownloaded FilePath deriving (Eq)

-- Trim
trim :: String -> String
trim = f . f
      where f = reverse . dropWhile isSpace

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

userAgent :: String
userAgent = concat [ "cabal-install/", display Paths_cabal_install.version
                   , " (", display buildOS, "; ", display buildArch, ")"
                   ]

data HttpTransport = HttpTransport {
      getHttp :: URI -> Maybe String -> FilePath -> IO (Int, Maybe String),
      postHttp :: URI -> String -> Maybe (String, String) -> IO (Int, String),
      putHttpFile :: URI -> FilePath -> Maybe (String,String) -> IO (Int, String)
    }

setupTransportDb :: Verbosity -> ProgramDb -> IO ProgramDb
setupTransportDb verbosity conf = configureAllKnownPrograms verbosity $ addKnownPrograms progs conf
    where progs = map simpleProgram ["curl","wget","powershell","bitsadmin"]

configureTransport :: Verbosity -> ProgramDb -> IO HttpTransport
configureTransport verbosity initialConf = maybe (plainHttpTransport verbosity) return . pickTransport =<< setupTransportDb verbosity initialConf
    where
      pickTransport :: ProgramDb -> Maybe HttpTransport
      pickTransport conf =     fmap (curlTransport verbosity) (lookupProgram (simpleProgram "curl") conf)
                           <|> fmap (wgetTransport verbosity) (lookupProgram (simpleProgram "wget") conf)
                           <|> fmap (powershellTransport verbosity) (lookupProgram (simpleProgram "powershell") conf)

statusParseFail :: URI -> String -> IO a
statusParseFail uri r = die $ "Failed to download " ++ show uri ++ " : No Status Code could be parsed from Response: " ++ r

curlTransport :: Verbosity -> ConfiguredProgram -> HttpTransport
curlTransport verbosity prog = HttpTransport gethttp posthttp puthttpfile
  where
    gethttp uri etag destPath = parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog args)
      where args = [show uri,"-o",destPath,"-L","--write-out","%{http_code}","-A",userAgent]
                   ++ maybe [] (\t -> ["--header","If-None-Match: " ++ t]) etag
            parseResponse x = case readMaybe $ trim x of
              Just i -> return (i, Nothing) -- TODO extract real etag
              Nothing -> statusParseFail uri x

    posthttp = undefined

    puthttpfile uri path auth = parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog args)
      where
        args = [show uri,"-F","package=@"++path,"--write-out","%{http_code}","-A",userAgent]
               ++ maybe [] (\(u,p) -> ["--digest","-u",u++":"++p]) auth
        parseResponse x = case readMaybe . trim =<< listToMaybe . take 1 . reverse . lines =<< return x of
          Just i -> return (i,x) -- TODO extract error?
          Nothing -> statusParseFail uri x


wgetTransport :: Verbosity -> ConfiguredProgram -> HttpTransport
wgetTransport verbosity prog = HttpTransport gethttp posthttp puthttpfile
  where
    gethttp uri etag destPath = parseResponse . snd =<< getProgramInvocationOutputAndErrors verbosity (programInvocation prog args)
      where
        args = ["-S",show uri,"--output-document="++destPath,"--user-agent="++userAgent]
               ++ maybe [] (\t -> ["--header","If-None-Match: " ++ t]) etag
        parseResponse x =
          let resp = reverse . takeUntil ("HTTP/" `isPrefixOf`) . reverse . map (dropWhile isSpace) . lines $ x
          in case readMaybe =<< listToMaybe . drop 1 . words =<< listToMaybe resp of
            Just i -> return (i, Nothing) --TODO etags
            Nothing -> statusParseFail uri x

    posthttp = undefined

    puthttpfile uri path auth = withTempFile (takeDirectory path) (takeFileName path) $ \tmpFile tmpHandle -> do
      boundary <- genBoundary
      body <- generateMultipartBody (ByteString.pack boundary) path
      ByteString.hPut tmpHandle body
      let args = ["-S",show uri,"--user-agent="++userAgent,"--post-file="++tmpFile]
                 ++ ["--header=\"++Content-type: multipart/form-data boundary="++boundary++"\""]
                 ++ maybe [] (\(u,p) -> ["--http-user="++u,"--http-password="++p]) auth

          parseResponse x =
            let resp = reverse . takeUntil ("HTTP/" `isPrefixOf`) . reverse . map (dropWhile isSpace) . lines $ x
            in case readMaybe =<< listToMaybe . drop 1 . words =<< listToMaybe resp of
              Just i -> return (i, x)
              Nothing -> statusParseFail uri x
      parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog args)

    takeUntil _ [] = []
    takeUntil p (x:xs) = if p x then [x] else x : takeUntil p xs

powershellTransport :: Verbosity -> ConfiguredProgram -> HttpTransport
powershellTransport verbosity prog = HttpTransport gethttp posthttp puthttpfile
  where
    gethttp uri etag destPath = do
      proxyInfo <- proxy verbosity
      let
        escape x = '"' : x ++ "\"" --TODO write/find real escape.
        proxySettings = [] --TODO extract real settings from proxyInfo

        parseResponse x = case readMaybe $ trim x of
          Just i -> return (i, Nothing) -- TODO extract real etag
          Nothing -> statusParseFail uri x

        script = unlines . map (++";") $
                 ["$wc = new-object system.net.webclient",
                  "$wc.Headers.Add(\"user-agent\","++escape userAgent++")"]
                 ++ maybe [] (\t -> ["$wc.Headers.Add(\"If-None-Match " ++ t ++ ")"]) etag
                 ++ proxySettings
                 ++ ["$wc.DownloadFile("++ escape (show uri) ++ "," ++ escape destPath ++ ")",
                     "$wc.ResponseHeaders.Item(\"status\")"]

      parseResponse =<< getProgramInvocationOutput verbosity (programInvocation prog [script])

    posthttp = undefined

    puthttpfile uri path auth = error "powershell upload not yet enabled" --TODO

-- TODO bitsadmin?

-- todo ensure an explicit flag to allow plain insecure http
plainHttpTransport :: Verbosity -> IO HttpTransport
plainHttpTransport verbosity = return $ HttpTransport gethttp posthttp puthttpfile
  where gethttp uri etag destPath =
          processGetResult destPath . snd =<< cabalBrowse (request
            Request{ rqURI     = uri
                   , rqMethod  = GET
                   , rqHeaders = Header HdrUserAgent userAgent
                                 : maybe [] (\t -> [Header HdrIfNoneMatch t]) etag
                   , rqBody    = ByteString.empty })

        processGetResult destPath resp = do
            when (code==200) $ writeFileAtomic destPath $ rspBody resp
            return (code, etag)
          where code = case rspCode (resp) of (a,b,c) -> a*100 + b*10 + c
                etag = lookupHeader HdrETag (rspHeaders resp)

        posthttp = undefined

        puthttpfile uri path auth = do
          boundary <- genBoundary
          body <- generateMultipartBody (ByteString.pack boundary) path
          let authorize = do
                setAllowBasicAuth False
                setAuthorityGen (\_ _ -> return auth)
          processPutResult . snd <$> cabalBrowse (authorize >> request Request {
                         rqURI = uri,
                         rqMethod = POST,
                         rqHeaders = [Header HdrContentType ("multipart/form-data; boundary="++boundary),
                                      Header HdrContentLength (show (ByteString.length body)),
                                      Header HdrAccept ("text/plain")],
                         rqBody = body
                        })

        processPutResult resp = (code, rspReason resp)
          where code = case rspCode (resp) of (a,b,c) -> a*100 + b*10 + c

        cabalBrowse act = do
          p <- proxy verbosity
          Exception.handleJust
                (guard . isDoesNotExistError)
                (const . die $ "Couldn't establish HTTP connection. "
                 ++ "Possible cause: HTTP proxy server is down.") $
                browse $ do
                  setProxy p
                  setErrHandler (warn verbosity . ("http error: "++))
                  setOutHandler (debug verbosity)
                  act

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

  transport <- configureTransport verbosity =<< setupTransportDb verbosity defaultProgramDb
  result <- getHttp transport uri etag tmpFile

  -- Only write the etag if we get a 200 response code.
  -- A 304 still sends us an etag header.
  case result of
    (200, Just newEtag) -> writeFile etagPath newEtag
    _ -> return ()

  case fst result of
    200 -> do
        info verbosity ("Downloaded to " ++ path)
        renameFile tmpFile path
        return (FileDownloaded path)
    304 -> do
        notice verbosity "Skipping download: Local and remote files match."
        return FileAlreadyInCache
    errCode ->  die $ "Failed to download " ++ show uri ++ " : HTTP code " ++ show errCode

uploadToURI :: Verbosity -> URI -> FilePath -> Maybe (String,String) -> IO (Int, String)
uploadToURI verbosity uri path auth = do
  transport <- configureTransport verbosity =<< setupTransportDb verbosity defaultProgramDb
  putHttpFile transport uri path auth


-- Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri
    = case uriAuthority uri of
        Just (URIAuth {uriRegName = "hackage.haskell.org"}) ->
            FilePath.Posix.splitDirectories (uriPath uri) == ["/","packages","archive"]
        _ -> False

-- Gets us the temp file name but gives us more control over the file itself.

withTempFileName :: FilePath
             -> String
             -> (FilePath -> IO a) -> IO a
withTempFileName tmpDir template action =
  Exception.bracket
    (openTempFile tmpDir template)
    (\(name, _) -> (`when` removeFile name) =<< doesFileExist name)
    (\(name, h) -> hClose h >> action name)

-- Multipart stuff partially taken from cgi package.

genBoundary :: IO String
genBoundary = do i <- randomRIO (0x10000000000000,0xFFFFFFFFFFFFFF) :: IO Integer
                 return $ showHex i ""

generateMultipartBody :: ByteString.ByteString -> FilePath -> IO ByteString.ByteString
generateMultipartBody boundary path = do
  pkg <- ByteString.readFile path
  let
    crlf = ByteString.pack "\r\n"
    dd = ByteString.pack "--"
    printOneMultiPart (hs, c) = printBodyPart (hs,c) ++ [crlf, dd, boundary, dd, crlf]
    printBodyPart (hs, c) = [crlf, dd, boundary, crlf] ++ map (ByteString.pack . show) hs ++ [crlf, c]
    formData = ( [Header (HdrCustom "Content-disposition") $
                   "form-data; name=package; filename=\""++takeFileName path++"\"",
                   Header HdrContentType "application/x-gzip"],
                   pkg)
    body = ByteString.concat $ printOneMultiPart formData
  return body

-- This should go back in the main program machinery. We need the errors explicitly because wget writes its results to stderr for no good reason.

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

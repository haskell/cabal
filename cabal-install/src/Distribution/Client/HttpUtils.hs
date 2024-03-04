{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- | Separate module for HTTP actions, using a proxy server if one exists.
module Distribution.Client.HttpUtils
  ( DownloadResult (..)
  , configureTransport
  , HttpTransport (..)
  , HttpCode
  , downloadURI
  , transportCheckHttps
  , remoteRepoCheckHttps
  , remoteRepoTryUpgradeToHttps
  , isOldHackageURI
  ) where

import Distribution.Client.Compat.Prelude hiding (Proxy (..))
import Distribution.Utils.Generic
import Prelude ()

import qualified Control.Exception as Exception
import Distribution.Client.Types
  ( RemoteRepo (..)
  , unRepoName
  )
import Distribution.Client.Types.Credentials (Auth)
import Distribution.Client.Utils
  ( withTempFileName
  )
import Distribution.Client.Version
  ( cabalInstallVersion
  )
import Distribution.Simple.Program
  ( ConfiguredProgram
  , Program
  , ProgramInvocation (..)
  , getProgramInvocationOutput
  , programInvocation
  , programPath
  , simpleProgram
  )
import Distribution.Simple.Program.Db
  ( ProgramDb
  , addKnownPrograms
  , configureAllKnownPrograms
  , emptyProgramDb
  , lookupProgram
  , prependProgramSearchPath
  , requireProgram
  )
import Distribution.Simple.Program.Run
  ( getProgramInvocationOutputAndErrors
  )
import Distribution.Simple.Utils
  ( IOData (..)
  , copyFileVerbose
  , debug
  , dieWithException
  , info
  , notice
  , warn
  , withTempFile
  )
import Distribution.System
  ( buildArch
  , buildOS
  )
import Distribution.Utils.String (trim)
import Network.Browser
  ( browse
  , request
  , setAllowBasicAuth
  , setAuthorityGen
  , setErrHandler
  , setOutHandler
  , setProxy
  , setUserAgent
  )
import Network.HTTP
  ( Header (..)
  , HeaderName (..)
  , Request (..)
  , RequestMethod (..)
  , Response (..)
  , lookupHeader
  )
import Network.HTTP.Proxy (Proxy (..), fetchProxy)
import Network.URI
  ( URI (..)
  , URIAuth (..)
  , uriToString
  )
import Numeric (showHex)
import System.Directory
  ( canonicalizePath
  , doesFileExist
  , renameFile
  )
import System.FilePath
  ( takeDirectory
  , takeFileName
  , (<.>)
  )
import qualified System.FilePath.Posix as FilePath.Posix
  ( splitDirectories
  )
import System.IO
  ( IOMode (ReadMode)
  , hClose
  , hGetContents
  , withFile
  )
import System.IO.Error
  ( isDoesNotExistError
  )
import System.Random (randomRIO)

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Char as Char
import Distribution.Client.Errors
import qualified Distribution.Compat.CharParsing as P

------------------------------------------------------------------------------
-- Downloading a URI, given an HttpTransport
--

data DownloadResult
  = FileAlreadyInCache
  | FileDownloaded FilePath
  deriving (Eq)

data DownloadCheck
  = -- | already downloaded and sha256 matches
    Downloaded
  | -- | already downloaded and we have etag
    CheckETag String
  | -- | needs download with optional hash check
    NeedsDownload (Maybe BS.ByteString)
  deriving (Eq)

downloadURI
  :: HttpTransport
  -> Verbosity
  -> URI
  -- ^ What to download
  -> FilePath
  -- ^ Where to put it
  -> IO DownloadResult
downloadURI _transport verbosity uri path | uriScheme uri == "file:" = do
  copyFileVerbose verbosity (uriPath uri) path
  return (FileDownloaded path)
-- Can we store the hash of the file so we can safely return path when the
-- hash matches to avoid unnecessary computation?

downloadURI transport verbosity uri path = do
  targetExists <- doesFileExist path

  downloadCheck <-
    -- if we have uriFrag, then we expect there to be #sha256=...
    if not (null uriFrag)
      then case sha256parsed of
        -- we know the hash, and target exists
        Right expected | targetExists -> do
          contents <- LBS.readFile path
          let actual = SHA256.hashlazy contents
          if expected == actual
            then return Downloaded
            else return (NeedsDownload (Just expected))

        -- we known the hash, target doesn't exist
        Right expected -> return (NeedsDownload (Just expected))
        -- we failed to parse uriFragment
        Left err ->
          dieWithException verbosity $ CannotParseURIFragment uriFrag err
      else -- if there are no uri fragment, use ETag
      do
        etagPathExists <- doesFileExist etagPath
        -- In rare cases the target file doesn't exist, but the etag does.
        if targetExists && etagPathExists
          then return (CheckETag etagPath)
          else return (NeedsDownload Nothing)

  -- Only use the external http transports if we actually have to
  -- (or have been told to do so)
  let transport'
        | uriScheme uri == "http:"
        , not (transportManuallySelected transport) =
            plainHttpTransport
        | otherwise =
            transport

  case downloadCheck of
    Downloaded -> return FileAlreadyInCache
    CheckETag etag -> makeDownload transport' Nothing (Just etag)
    NeedsDownload hash -> makeDownload transport' hash Nothing
  where
    makeDownload :: HttpTransport -> Maybe BS8.ByteString -> Maybe String -> IO DownloadResult
    makeDownload transport' sha256 etag = withTempFileName (takeDirectory path) (takeFileName path) $ \tmpFile -> do
      result <- getHttp transport' verbosity uri etag tmpFile []

      -- Only write the etag if we get a 200 response code.
      -- A 304 still sends us an etag header.
      case result of
        -- if we have hash, we don't care about etag.
        (200, _) | Just expected <- sha256 -> do
          contents <- LBS.readFile tmpFile
          let actual = SHA256.hashlazy contents
          unless (actual == expected) $
            dieWithException verbosity $
              MakeDownload uri expected actual
        (200, Just newEtag) -> writeFile etagPath newEtag
        _ -> return ()

      case fst result of
        200 -> do
          info verbosity ("Downloaded to " ++ path)
          renameFile tmpFile path
          return (FileDownloaded path)
        304 -> do
          notice verbosity "Skipping download: local and remote files match."
          return FileAlreadyInCache
        errCode ->
          dieWithException verbosity $ FailedToDownloadURI uri (show errCode)

    etagPath = path <.> "etag"
    uriFrag = uriFragment uri

    sha256parsed :: Either String BS.ByteString
    sha256parsed = explicitEitherParsec fragmentParser uriFrag

    fragmentParser = do
      _ <- P.string "#sha256="
      str <- some P.hexDigit
      let bs = Base16.decode (BS8.pack str)
#if MIN_VERSION_base16_bytestring(1,0,0)
      either fail return bs
#else
      return (fst bs)
#endif

------------------------------------------------------------------------------
-- Utilities for repo url management
--

remoteRepoCheckHttps :: Verbosity -> HttpTransport -> RemoteRepo -> IO ()
remoteRepoCheckHttps verbosity transport repo
  | uriScheme (remoteRepoURI repo) == "https:"
  , not (transportSupportsHttps transport) =
      dieWithException verbosity $ RemoteRepoCheckHttps (unRepoName (remoteRepoName repo)) requiresHttpsErrorMessage
  | otherwise = return ()

transportCheckHttps :: Verbosity -> HttpTransport -> URI -> IO ()
transportCheckHttps verbosity transport uri
  | uriScheme uri == "https:"
  , not (transportSupportsHttps transport) =
      dieWithException verbosity $ TransportCheckHttps uri requiresHttpsErrorMessage
  | otherwise = return ()

requiresHttpsErrorMessage :: String
requiresHttpsErrorMessage =
  "requires HTTPS however the built-in HTTP implementation "
    ++ "does not support HTTPS. The transport implementations with HTTPS "
    ++ "support are "
    ++ intercalate
      ", "
      [name | (name, _, True, _) <- supportedTransports]
    ++ ". One of these will be selected automatically if the corresponding "
    ++ "external program is available, or one can be selected specifically "
    ++ "with the global flag --http-transport="

remoteRepoTryUpgradeToHttps :: Verbosity -> HttpTransport -> RemoteRepo -> IO RemoteRepo
remoteRepoTryUpgradeToHttps verbosity transport repo
  | remoteRepoShouldTryHttps repo
  , uriScheme (remoteRepoURI repo) == "http:"
  , not (transportSupportsHttps transport)
  , not (transportManuallySelected transport) =
      dieWithException verbosity $ TryUpgradeToHttps [name | (name, _, True, _) <- supportedTransports]
  | remoteRepoShouldTryHttps repo
  , uriScheme (remoteRepoURI repo) == "http:"
  , transportSupportsHttps transport =
      return
        repo
          { remoteRepoURI = (remoteRepoURI repo){uriScheme = "https:"}
          }
  | otherwise =
      return repo

-- | Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri =
  case uriAuthority uri of
    Just (URIAuth{uriRegName = "hackage.haskell.org"}) ->
      FilePath.Posix.splitDirectories (uriPath uri)
        == ["/", "packages", "archive"]
    _ -> False

------------------------------------------------------------------------------
-- Setting up a HttpTransport
--

data HttpTransport = HttpTransport
  { getHttp
      :: Verbosity
      -> URI
      -> Maybe ETag
      -> FilePath
      -> [Header]
      -> IO (HttpCode, Maybe ETag)
  -- ^ GET a URI, with an optional ETag (to do a conditional fetch),
  -- write the resource to the given file and return the HTTP status code,
  -- and optional ETag.
  , postHttp
      :: Verbosity
      -> URI
      -> String
      -> Maybe Auth
      -> IO (HttpCode, String)
  -- ^ POST a resource to a URI, with optional 'Auth'
  -- and return the HTTP status code and any redirect URL.
  , postHttpFile
      :: Verbosity
      -> URI
      -> FilePath
      -> Maybe Auth
      -> IO (HttpCode, String)
  -- ^ POST a file resource to a URI using multipart\/form-data encoding,
  -- with optional 'Auth' and return the HTTP status
  -- code and any error string.
  , putHttpFile
      :: Verbosity
      -> URI
      -> FilePath
      -> Maybe Auth
      -> [Header]
      -> IO (HttpCode, String)
  -- ^ PUT a file resource to a URI, with optional 'Auth',
  -- extra headers and return the HTTP status code
  -- and any error string.
  , transportSupportsHttps :: Bool
  -- ^ Whether this transport supports https or just http.
  , transportManuallySelected :: Bool
  -- ^ Whether this transport implementation was specifically chosen by
  -- the user via configuration, or whether it was automatically selected.
  -- Strictly speaking this is not a property of the transport itself but
  -- about how it was chosen. Nevertheless it's convenient to keep here.
  }

-- TODO: why does postHttp return a redirect, but postHttpFile return errors?

type HttpCode = Int
type ETag = String

noPostYet
  :: Verbosity
  -> URI
  -> String
  -> Maybe Auth
  -> IO (Int, String)
noPostYet verbosity _ _ _ = dieWithException verbosity NoPostYet

supportedTransports
  :: [ ( String
       , Maybe Program
       , Bool
       , ProgramDb -> Maybe HttpTransport
       )
     ]
supportedTransports =
  [ let prog = simpleProgram "curl"
     in ( "curl"
        , Just prog
        , True
        , \db -> curlTransport <$> lookupProgram prog db
        )
  , let prog = simpleProgram "wget"
     in ( "wget"
        , Just prog
        , True
        , \db -> wgetTransport <$> lookupProgram prog db
        )
  , let prog = simpleProgram "powershell"
     in ( "powershell"
        , Just prog
        , True
        , \db -> powershellTransport <$> lookupProgram prog db
        )
  ,
    ( "plain-http"
    , Nothing
    , False
    , \_ -> Just plainHttpTransport
    )
  ]

configureTransport :: Verbosity -> [FilePath] -> Maybe String -> IO HttpTransport
configureTransport verbosity extraPath (Just name) =
  -- the user specifically selected a transport by name so we'll try and
  -- configure that one

  case find (\(name', _, _, _) -> name' == name) supportedTransports of
    Just (_, mprog, _tls, mkTrans) -> do
      baseProgDb <- prependProgramSearchPath verbosity extraPath emptyProgramDb
      progdb <- case mprog of
        Nothing -> return emptyProgramDb
        Just prog -> snd <$> requireProgram verbosity prog baseProgDb
      --      ^^ if it fails, it'll fail here

      let transport = fromMaybe (error "configureTransport: failed to make transport") $ mkTrans progdb
      return transport{transportManuallySelected = True}
    Nothing ->
      dieWithException verbosity $ UnknownHttpTransportSpecified name [name' | (name', _, _, _) <- supportedTransports]
configureTransport verbosity extraPath Nothing = do
  -- the user hasn't selected a transport, so we'll pick the first one we
  -- can configure successfully, provided that it supports tls

  -- for all the transports except plain-http we need to try and find
  -- their external executable
  baseProgDb <- prependProgramSearchPath verbosity extraPath emptyProgramDb
  progdb <-
    configureAllKnownPrograms verbosity $
      addKnownPrograms
        [prog | (_, Just prog, _, _) <- supportedTransports]
        baseProgDb

  let availableTransports =
        [ (name, transport)
        | (name, _, _, mkTrans) <- supportedTransports
        , transport <- maybeToList (mkTrans progdb)
        ]
  let (name, transport) =
        fromMaybe ("plain-http", plainHttpTransport) (safeHead availableTransports)
  debug verbosity $ "Selected http transport implementation: " ++ name

  return transport{transportManuallySelected = False}

------------------------------------------------------------------------------
-- The HttpTransports based on external programs
--

curlTransport :: ConfiguredProgram -> HttpTransport
curlTransport prog =
  HttpTransport gethttp posthttp posthttpfile puthttpfile True False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
      withTempFile
        (takeDirectory destPath)
        "curl-headers.txt"
        $ \tmpFile tmpHandle -> do
          hClose tmpHandle
          let args =
                [ show uri
                , "--output"
                , destPath
                , "--location"
                , "--write-out"
                , "%{http_code}"
                , "--user-agent"
                , userAgent
                , "--silent"
                , "--show-error"
                , "--dump-header"
                , tmpFile
                ]
                  ++ concat
                    [ ["--header", "If-None-Match: " ++ t]
                    | t <- maybeToList etag
                    ]
                  ++ concat
                    [ ["--header", show name ++ ": " ++ value]
                    | Header name value <- reqHeaders
                    ]

          resp <-
            getProgramInvocationOutput verbosity $
              addAuthConfig
                Nothing
                uri
                (programInvocation prog args)

          withFile tmpFile ReadMode $ \hnd -> do
            headers <- hGetContents hnd
            (code, _err, etag') <- parseResponse verbosity uri resp headers
            evaluate $ force (code, etag')

    posthttp = noPostYet

    addAuthConfig explicitAuth uri progInvocation = do
      -- attempt to derive a u/p pair from the uri authority if one exists
      -- all `uriUserInfo` values have '@' as a suffix. drop it.
      let uriDerivedAuth = case uriAuthority uri of
            (Just (URIAuth u _ _)) | not (null u) -> Just $ filter (/= '@') u
            _ -> Nothing
      -- prefer passed in auth to auth derived from uri. If neither exist, then no auth
      let mbAuthStringToken = case (explicitAuth, uriDerivedAuth) of
            (Just (Right token), _) -> Just $ Right token
            (Just (Left (uname, passwd)), _) -> Just $ Left (uname ++ ":" ++ passwd)
            (Nothing, Just a) -> Just $ Left a
            (Nothing, Nothing) -> Nothing
      case mbAuthStringToken of
        Just (Left up) ->
          progInvocation
            { progInvokeInput =
                Just . IODataText . unlines $
                  [ "--digest"
                  , "--user " ++ up
                  ]
            , progInvokeArgs = ["--config", "-"] ++ progInvokeArgs progInvocation
            }
        Just (Right token) ->
          progInvocation
            { progInvokeArgs =
                ["--header", "Authorization: X-ApiKey " ++ token]
                  ++ progInvokeArgs progInvocation
            }
        Nothing -> progInvocation

    posthttpfile verbosity uri path auth = do
      let args =
            [ show uri
            , "--form"
            , "package=@" ++ path
            , "--write-out"
            , "\n%{http_code}"
            , "--user-agent"
            , userAgent
            , "--silent"
            , "--show-error"
            , "--header"
            , "Accept: text/plain"
            , "--location"
            ]
      resp <-
        getProgramInvocationOutput verbosity $
          addAuthConfig
            auth
            uri
            (programInvocation prog args)
      (code, err, _etag) <- parseResponse verbosity uri resp ""
      return (code, err)

    puthttpfile verbosity uri path auth headers = do
      let args =
            [ show uri
            , "--request"
            , "PUT"
            , "--data-binary"
            , "@" ++ path
            , "--write-out"
            , "\n%{http_code}"
            , "--user-agent"
            , userAgent
            , "--silent"
            , "--show-error"
            , "--location"
            , "--header"
            , "Accept: text/plain"
            ]
              ++ concat
                [ ["--header", show name ++ ": " ++ value]
                | Header name value <- headers
                ]
      resp <-
        getProgramInvocationOutput verbosity $
          addAuthConfig
            auth
            uri
            (programInvocation prog args)
      (code, err, _etag) <- parseResponse verbosity uri resp ""
      return (code, err)

    -- on success these curl invocations produces an output like "200"
    -- and on failure it has the server error response first
    parseResponse :: Verbosity -> URI -> String -> String -> IO (Int, String, Maybe ETag)
    parseResponse verbosity uri resp headers =
      let codeerr =
            case reverse (lines resp) of
              (codeLine : rerrLines) ->
                case readMaybe (trim codeLine) of
                  Just i ->
                    let errstr = mkErrstr rerrLines
                     in Just (i, errstr)
                  Nothing -> Nothing
              [] -> Nothing

          mkErrstr = unlines . reverse . dropWhile (all isSpace)

          mb_etag :: Maybe ETag
          mb_etag =
            listToMaybe $
              reverse
                [ etag
                | [name, etag] <- map words (lines headers)
                , isETag name
                ]
       in case codeerr of
            Just (i, err) -> return (i, err, mb_etag)
            _ -> statusParseFail verbosity uri resp

wgetTransport :: ConfiguredProgram -> HttpTransport
wgetTransport prog =
  HttpTransport gethttp posthttp posthttpfile puthttpfile True False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
      resp <- runWGet verbosity uri args

      -- wget doesn't support range requests.
      -- so, we not only ignore range request headers,
      -- but we also display a warning message when we see them.
      let hasRangeHeader = any isRangeHeader reqHeaders
          warningMsg =
            "the 'wget' transport currently doesn't support"
              ++ " range requests, which wastes network bandwidth."
              ++ " To fix this, set 'http-transport' to 'curl' or"
              ++ " 'plain-http' in '~/.config/cabal/config'."
              ++ " Note that the 'plain-http' transport doesn't"
              ++ " support HTTPS.\n"

      when (hasRangeHeader) $ warn verbosity warningMsg
      (code, etag') <- parseOutput verbosity uri resp
      return (code, etag')
      where
        args =
          [ "--output-document=" ++ destPath
          , "--user-agent=" ++ userAgent
          , "--tries=5"
          , "--timeout=15"
          , "--server-response"
          ]
            ++ concat
              [ ["--header", "If-None-Match: " ++ t]
              | t <- maybeToList etag
              ]
            ++ [ "--header=" ++ show name ++ ": " ++ value
               | hdr@(Header name value) <- reqHeaders
               , (not (isRangeHeader hdr))
               ]

        -- wget doesn't support range requests.
        -- so, we ignore range request headers, lest we get errors.
        isRangeHeader :: Header -> Bool
        isRangeHeader (Header HdrRange _) = True
        isRangeHeader _ = False

    posthttp = noPostYet

    posthttpfile verbosity uri path auth =
      withTempFile
        (takeDirectory path)
        (takeFileName path)
        $ \tmpFile tmpHandle ->
          withTempFile (takeDirectory path) "response" $
            \responseFile responseHandle -> do
              hClose responseHandle
              (body, boundary) <- generateMultipartBody path
              LBS.hPut tmpHandle body
              hClose tmpHandle
              let args =
                    [ "--post-file=" ++ tmpFile
                    , "--user-agent=" ++ userAgent
                    , "--server-response"
                    , "--output-document=" ++ responseFile
                    , "--header=Accept: text/plain"
                    , "--header=Content-type: multipart/form-data; "
                        ++ "boundary="
                        ++ boundary
                    ]
                      ++ maybeToList (authTokenHeader auth)
              out <- runWGet verbosity (addUriAuth auth uri) args
              (code, _etag) <- parseOutput verbosity uri out
              withFile responseFile ReadMode $ \hnd -> do
                resp <- hGetContents hnd
                evaluate $ force (code, resp)

    puthttpfile verbosity uri path auth headers =
      withTempFile (takeDirectory path) "response" $
        \responseFile responseHandle -> do
          hClose responseHandle
          let args =
                [ "--method=PUT"
                , "--body-file=" ++ path
                , "--user-agent=" ++ userAgent
                , "--server-response"
                , "--output-document=" ++ responseFile
                , "--header=Accept: text/plain"
                ]
                  ++ [ "--header=" ++ show name ++ ": " ++ value
                     | Header name value <- headers
                     ]
                  ++ maybeToList (authTokenHeader auth)

          out <- runWGet verbosity (addUriAuth auth uri) args
          (code, _etag) <- parseOutput verbosity uri out
          withFile responseFile ReadMode $ \hnd -> do
            resp <- hGetContents hnd
            evaluate $ force (code, resp)

    authTokenHeader (Just (Right token)) = Just $ "--header=Authorization: X-ApiKey " ++ token
    authTokenHeader _ = Nothing

    addUriAuth (Just (Left (user, pass))) uri =
      uri
        { uriAuthority = Just a{uriUserInfo = user ++ ":" ++ pass ++ "@"}
        }
      where
        a = fromMaybe (URIAuth "" "" "") (uriAuthority uri)
    addUriAuth _ uri = uri

    runWGet verbosity uri args = do
      -- We pass the URI via STDIN because it contains the users' credentials
      -- and sensitive data should not be passed via command line arguments.
      let
        invocation =
          (programInvocation prog ("--input-file=-" : args))
            { progInvokeInput = Just $ IODataText $ uriToString id uri ""
            }

      -- wget returns its output on stderr rather than stdout
      (_, resp, exitCode) <-
        getProgramInvocationOutputAndErrors
          verbosity
          invocation
      -- wget returns exit code 8 for server "errors" like "304 not modified"
      if exitCode == ExitSuccess || exitCode == ExitFailure 8
        then return resp
        else dieWithException verbosity $ WGetServerError (programPath prog) resp

    -- With the --server-response flag, wget produces output with the full
    -- http server response with all headers, we want to find a line like
    -- "HTTP/1.1 200 OK", but only the last one, since we can have multiple
    -- requests due to redirects.
    parseOutput verbosity uri resp =
      let parsedCode =
            listToMaybe
              [ code
              | (protocol : codestr : _err) <- map words (reverse (lines resp))
              , "HTTP/" `isPrefixOf` protocol
              , code <- maybeToList (readMaybe codestr)
              ]
          mb_etag :: Maybe ETag
          mb_etag =
            listToMaybe
              [ etag
              | [name, etag] <- map words (reverse (lines resp))
              , isETag name
              ]
       in case parsedCode of
            Just i -> return (i, mb_etag)
            _ -> statusParseFail verbosity uri resp

powershellTransport :: ConfiguredProgram -> HttpTransport
powershellTransport prog =
  HttpTransport gethttp posthttp posthttpfile puthttpfile True False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
      resp <-
        runPowershellScript verbosity $
          webclientScript
            (escape (show uri))
            ( ("$targetStream = New-Object -TypeName System.IO.FileStream -ArgumentList " ++ (escape destPath) ++ ", Create")
                : (setupHeaders ((useragentHeader : etagHeader) ++ reqHeaders))
            )
            [ "$response = $request.GetResponse()"
            , "$responseStream = $response.GetResponseStream()"
            , "$buffer = new-object byte[] 10KB"
            , "$count = $responseStream.Read($buffer, 0, $buffer.length)"
            , "while ($count -gt 0)"
            , "{"
            , "    $targetStream.Write($buffer, 0, $count)"
            , "    $count = $responseStream.Read($buffer, 0, $buffer.length)"
            , "}"
            , "Write-Host ($response.StatusCode -as [int]);"
            , "Write-Host $response.GetResponseHeader(\"ETag\").Trim('\"')"
            ]
            [ "$targetStream.Flush()"
            , "$targetStream.Close()"
            , "$targetStream.Dispose()"
            , "$responseStream.Dispose()"
            ]
      parseResponse resp
      where
        parseResponse :: String -> IO (HttpCode, Maybe ETag)
        parseResponse x =
          case lines $ trim x of
            (code : etagv : _) -> fmap (\c -> (c, Just etagv)) $ parseCode code x
            (code : _) -> fmap (\c -> (c, Nothing)) $ parseCode code x
            _ -> statusParseFail verbosity uri x
        parseCode :: String -> String -> IO HttpCode
        parseCode code x = case readMaybe code of
          Just i -> return i
          Nothing -> statusParseFail verbosity uri x
        etagHeader = [Header HdrIfNoneMatch t | t <- maybeToList etag]

    posthttp = noPostYet

    posthttpfile verbosity uri path auth =
      withTempFile
        (takeDirectory path)
        (takeFileName path)
        $ \tmpFile tmpHandle -> do
          (body, boundary) <- generateMultipartBody path
          LBS.hPut tmpHandle body
          hClose tmpHandle
          fullPath <- canonicalizePath tmpFile

          let contentHeader =
                Header
                  HdrContentType
                  ("multipart/form-data; boundary=" ++ boundary)
          resp <-
            runPowershellScript verbosity $
              webclientScript
                (escape (show uri))
                (setupHeaders (contentHeader : extraHeaders) ++ setupAuth auth)
                (uploadFileAction "POST" uri fullPath)
                uploadFileCleanup
          parseUploadResponse verbosity uri resp

    puthttpfile verbosity uri path auth headers = do
      fullPath <- canonicalizePath path
      resp <-
        runPowershellScript verbosity $
          webclientScript
            (escape (show uri))
            (setupHeaders (extraHeaders ++ headers) ++ setupAuth auth)
            (uploadFileAction "PUT" uri fullPath)
            uploadFileCleanup
      parseUploadResponse verbosity uri resp

    runPowershellScript verbosity script = do
      let args =
            [ "-InputFormat"
            , "None"
            , -- the default execution policy doesn't allow running
              -- unsigned scripts, so we need to tell powershell to bypass it
              "-ExecutionPolicy"
            , "bypass"
            , "-NoProfile"
            , "-NonInteractive"
            , "-Command"
            , "-"
            ]
      debug verbosity script
      getProgramInvocationOutput
        verbosity
        (programInvocation prog args)
          { progInvokeInput = Just $ IODataText $ script ++ "\nExit(0);"
          }

    escape = show

    useragentHeader = Header HdrUserAgent userAgent
    extraHeaders = [Header HdrAccept "text/plain", useragentHeader]

    setupHeaders headers =
      [ "$request." ++ addHeader name value
      | Header name value <- headers
      ]
      where
        addHeader header value =
          case header of
            HdrAccept -> "Accept = " ++ escape value
            HdrUserAgent -> "UserAgent = " ++ escape value
            HdrConnection -> "Connection = " ++ escape value
            HdrContentLength -> "ContentLength = " ++ escape value
            HdrContentType -> "ContentType = " ++ escape value
            HdrDate -> "Date = " ++ escape value
            HdrExpect -> "Expect = " ++ escape value
            HdrHost -> "Host = " ++ escape value
            HdrIfModifiedSince -> "IfModifiedSince = " ++ escape value
            HdrReferer -> "Referer = " ++ escape value
            HdrTransferEncoding -> "TransferEncoding = " ++ escape value
            HdrRange ->
              let (start, end) =
                    if "bytes=" `isPrefixOf` value
                      then case break (== '-') value' of
                        (start', '-' : end') -> (start', end')
                        _ -> error $ "Could not decode range: " ++ value
                      else error $ "Could not decode range: " ++ value
                  value' = drop 6 value
               in "AddRange(\"bytes\", " ++ escape start ++ ", " ++ escape end ++ ");"
            name -> "Headers.Add(" ++ escape (show name) ++ "," ++ escape value ++ ");"

    setupAuth (Just (Left (uname, passwd))) =
      [ "$request.Credentials = new-object System.Net.NetworkCredential("
          ++ escape uname
          ++ ","
          ++ escape passwd
          ++ ",\"\");"
      ]
    setupAuth (Just (Right token)) =
      ["$request.Headers[\"Authorization\"] = " ++ escape ("X-ApiKey " ++ token)]
    setupAuth Nothing = []

    uploadFileAction method _uri fullPath =
      [ "$request.Method = " ++ show method
      , "$requestStream = $request.GetRequestStream()"
      , "$fileStream = [System.IO.File]::OpenRead(" ++ escape fullPath ++ ")"
      , "$bufSize=10000"
      , "$chunk = New-Object byte[] $bufSize"
      , "while( $bytesRead = $fileStream.Read($chunk,0,$bufsize) )"
      , "{"
      , "  $requestStream.write($chunk, 0, $bytesRead)"
      , "  $requestStream.Flush()"
      , "}"
      , ""
      , "$responseStream = $request.getresponse()"
      , "$responseReader = new-object System.IO.StreamReader $responseStream.GetResponseStream()"
      , "$code = $response.StatusCode -as [int]"
      , "if ($code -eq 0) {"
      , "  $code = 200;"
      , "}"
      , "Write-Host $code"
      , "Write-Host $responseReader.ReadToEnd()"
      ]

    uploadFileCleanup =
      [ "$fileStream.Close()"
      , "$requestStream.Close()"
      , "$responseStream.Close()"
      ]

    parseUploadResponse verbosity uri resp = case lines (trim resp) of
      (codeStr : message)
        | Just code <- readMaybe codeStr -> return (code, unlines message)
      _ -> statusParseFail verbosity uri resp

    webclientScript uri setup action cleanup =
      unlines
        [ "[Net.ServicePointManager]::SecurityProtocol = \"tls12, tls11, tls\""
        , "$uri = New-Object \"System.Uri\" " ++ uri
        , "$request = [System.Net.HttpWebRequest]::Create($uri)"
        , unlines setup
        , "Try {"
        , unlines (map ("  " ++) action)
        , "} Catch [System.Net.WebException] {"
        , "  $exception = $_.Exception;"
        , "  If ($exception.Status -eq "
            ++ "[System.Net.WebExceptionStatus]::ProtocolError) {"
        , "    $response = $exception.Response -as [System.Net.HttpWebResponse];"
        , "    $reader = new-object "
            ++ "System.IO.StreamReader($response.GetResponseStream());"
        , "    Write-Host ($response.StatusCode -as [int]);"
        , "    Write-Host $reader.ReadToEnd();"
        , "  } Else {"
        , "    Write-Host $exception.Message;"
        , "  }"
        , "} Catch {"
        , "  Write-Host $_.Exception.Message;"
        , "} finally {"
        , unlines (map ("  " ++) cleanup)
        , "}"
        ]

------------------------------------------------------------------------------
-- The builtin plain HttpTransport
--

plainHttpTransport :: HttpTransport
plainHttpTransport =
  HttpTransport gethttp posthttp posthttpfile puthttpfile False False
  where
    gethttp verbosity uri etag destPath reqHeaders = do
      let req =
            Request
              { rqURI = uri
              , rqMethod = GET
              , rqHeaders =
                  [ Header HdrIfNoneMatch t
                  | t <- maybeToList etag
                  ]
                    ++ reqHeaders
              , rqBody = LBS.empty
              }
      (_, resp) <- cabalBrowse verbosity Nothing (request req)
      let code = convertRspCode (rspCode resp)
          etag' = lookupHeader HdrETag (rspHeaders resp)
      -- 206 Partial Content is a normal response to a range request; see #3385.
      when (code == 200 || code == 206) $
        writeFileAtomic destPath $
          rspBody resp
      return (code, etag')

    posthttp = noPostYet

    posthttpfile verbosity uri path auth = do
      (body, boundary) <- generateMultipartBody path
      let headers =
            [ Header
                HdrContentType
                ("multipart/form-data; boundary=" ++ boundary)
            , Header HdrContentLength (show (LBS8.length body))
            , Header HdrAccept ("text/plain")
            ]
              ++ maybeToList (authTokenHeader auth)
          req =
            Request
              { rqURI = uri
              , rqMethod = POST
              , rqHeaders = headers
              , rqBody = body
              }
      (_, resp) <- cabalBrowse verbosity auth (request req)
      return (convertRspCode (rspCode resp), rspErrorString resp)

    puthttpfile verbosity uri path auth headers = do
      body <- LBS8.readFile path
      let req =
            Request
              { rqURI = uri
              , rqMethod = PUT
              , rqHeaders =
                  Header HdrContentLength (show (LBS8.length body))
                    : Header HdrAccept "text/plain"
                    : maybeToList (authTokenHeader auth)
                    ++ headers
              , rqBody = body
              }
      (_, resp) <- cabalBrowse verbosity auth (request req)
      return (convertRspCode (rspCode resp), rspErrorString resp)

    convertRspCode (a, b, c) = a * 100 + b * 10 + c

    rspErrorString resp =
      case lookupHeader HdrContentType (rspHeaders resp) of
        Just contenttype
          | takeWhile (/= ';') contenttype == "text/plain" ->
              LBS8.unpack (rspBody resp)
        _ -> rspReason resp

    cabalBrowse verbosity auth act = do
      p <- fixupEmptyProxy <$> fetchProxy True
      Exception.handleJust
        (guard . isDoesNotExistError)
        ( const . dieWithException verbosity $ Couldn'tEstablishHttpConnection
        )
        $ browse
        $ do
          setProxy p
          setErrHandler (warn verbosity . ("http error: " ++))
          setOutHandler (debug verbosity)
          setUserAgent userAgent
          setAllowBasicAuth False
          case auth of
            Just (Left x) -> setAuthorityGen (\_ _ -> return $ Just x)
            _ -> setAuthorityGen (\_ _ -> return Nothing)
          act

    authTokenHeader (Just (Right token)) = Just $ Header HdrAuthorization ("X-ApiKey " ++ token)
    authTokenHeader _ = Nothing

    fixupEmptyProxy (Proxy uri _) | null uri = NoProxy
    fixupEmptyProxy p = p

------------------------------------------------------------------------------
-- Common stuff used by multiple transport impls
--

userAgent :: String
userAgent =
  concat
    [ "cabal-install/"
    , prettyShow cabalInstallVersion
    , " ("
    , prettyShow buildOS
    , "; "
    , prettyShow buildArch
    , ")"
    ]

statusParseFail :: Verbosity -> URI -> String -> IO a
statusParseFail verbosity uri r =
  dieWithException verbosity $ StatusParseFail uri r

------------------------------------------------------------------------------
-- Multipart stuff partially taken from cgi package.
--

generateMultipartBody :: FilePath -> IO (LBS.ByteString, String)
generateMultipartBody path = do
  content <- LBS.readFile path
  boundary <- genBoundary
  let !body = formatBody content (LBS8.pack boundary)
  return (body, boundary)
  where
    formatBody content boundary =
      LBS8.concat $
        [crlf, dd, boundary, crlf]
          ++ [LBS8.pack (show header) | header <- headers]
          ++ [ crlf
             , content
             , crlf
             , dd
             , boundary
             , dd
             , crlf
             ]

    headers =
      [ Header
          (HdrCustom "Content-disposition")
          ( "form-data; name=package; "
              ++ "filename=\""
              ++ takeFileName path
              ++ "\""
          )
      , Header HdrContentType "application/x-gzip"
      ]

    crlf = LBS8.pack "\r\n"
    dd = LBS8.pack "--"

genBoundary :: IO String
genBoundary = do
  i <- randomRIO (0x10000000000000, 0xFFFFFFFFFFFFFF) :: IO Integer
  return $ showHex i ""

isETag :: String -> Bool
isETag name = fmap Char.toLower name == "etag:"

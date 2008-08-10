-- This is a quick hack for uploading packages to Hackage.
-- See http://hackage.haskell.org/trac/hackage/wiki/CabalUpload

module Distribution.Client.Upload (check, upload, report) where

import Distribution.Client.Types (Username(..), Password(..),Repo(..),RemoteRepo(..))
import Distribution.Client.HttpUtils (proxy, isOldHackageURI)

import Distribution.Simple.Utils (debug, notice, warn, info)
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Client.Config

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import qualified Distribution.Client.BuildReports.Upload as BuildReport
import qualified Distribution.Client.BuildReports.Storage as BuildReport

import Network.Browser
         ( BrowserAction, browse, request
         , Authority(..), addAuthority, setAuthorityGen
         , setOutHandler, setErrHandler, setProxy )
import Network.HTTP
         ( Header(..), HeaderName(..), findHeader
         , Request(..), RequestMethod(..), Response(..) )
import Network.URI (URI(uriPath), parseURI)

import Data.Char        (intToDigit)
import Numeric          (showHex)
import System.IO        (hFlush, stdin, stdout, hGetEcho, hSetEcho
                        ,openBinaryFile, IOMode(ReadMode), hGetContents)
import Control.Exception (bracket)
import System.Random    (randomRIO)
import System.FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import System.Directory
import Control.Monad (forM_)


--FIXME: how do we find this path for an arbitrary hackage server?
-- is it always at some fixed location relative to the server root?
legacyUploadURI :: URI
Just legacyUploadURI = parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/protected/upload-pkg"

checkURI :: URI
Just checkURI = parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/check-pkg"


upload :: Verbosity -> [Repo] -> Maybe Username -> Maybe Password -> [FilePath] -> IO ()
upload verbosity repos mUsername mPassword paths = do
          let uploadURI = if isOldHackageURI targetRepoURI
                          then legacyUploadURI
                          else targetRepoURI{uriPath = uriPath targetRepoURI `FilePath.Posix.combine` "upload"}
          Username username <- maybe promptUsername return mUsername
          Password password <- maybe promptPassword return mPassword
          let auth = addAuthority AuthBasic {
                       auRealm    = "Hackage",
                       auUsername = username,
                       auPassword = password,
                       auSite     = uploadURI
                     }
          flip mapM_ paths $ \path -> do
            notice verbosity $ "Uploading " ++ path ++ "... "
            handlePackage verbosity uploadURI auth path
  where
    targetRepoURI = remoteRepoURI $ selectUploadRepo [ remoteRepo | Left remoteRepo <- map repoKind repos ]
    promptUsername :: IO Username
    promptUsername = do
      putStr "Hackage username: "
      hFlush stdout
      fmap Username getLine

    promptPassword :: IO Password
    promptPassword = do
      putStr "Hackage password: "
      hFlush stdout
      -- save/restore the terminal echoing status
      bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
        hSetEcho stdin False  -- no echoing for entering the password
        fmap Password getLine

selectUploadRepo = last -- Use head?

report :: Verbosity -> [Repo] -> IO ()
report verbosity repos
    = forM_ repos $ \repo ->
      case repoKind repo of
        Left remoteRepo
            -> do dotCabal <- defaultCabalDir
                  let srcDir = dotCabal </> "reports" </> remoteRepoName remoteRepo
                  contents <- getDirectoryContents srcDir
                  forM_ (filter (\c -> takeExtension c == ".log") contents) $ \logFile ->
                      do inp <- readFile (srcDir </> logFile)
                         let (reportStr, buildLog) = read inp :: (String,String)
                         case BuildReport.parse reportStr of
                           Left errs -> do warn verbosity $ "Errors: " ++ errs -- FIXME
                           Right report ->
                               do info verbosity $ "Uploading report for " ++ display (BuildReport.package report)
                                  browse $ BuildReport.uploadReports (remoteRepoURI remoteRepo) [(report, Just buildLog)]
                                  return ()
        Right{} -> return ()

check :: Verbosity -> [FilePath] -> IO ()
check verbosity paths = do
          flip mapM_ paths $ \path -> do
            notice verbosity $ "Checking " ++ path ++ "... "
            handlePackage verbosity checkURI (return ()) path

handlePackage :: Verbosity -> URI -> BrowserAction () -> FilePath -> IO ()
handlePackage verbosity uri auth path =
  do req <- mkRequest uri path
     p   <- proxy verbosity
     debug verbosity $ "\n" ++ show req
     (_,resp) <- browse $ do
                   setProxy p
                   setErrHandler (warn verbosity . ("http error: "++))
                   setOutHandler (debug verbosity)
                   auth
                   setAuthorityGen (\_ _ -> return Nothing)
                   request req
     debug verbosity $ show resp
     case rspCode resp of
       (2,0,0) -> do notice verbosity "OK"
       (x,y,z) -> do notice verbosity $ "ERROR: " ++ path ++ ": " 
                                     ++ map intToDigit [x,y,z] ++ " "
                                     ++ rspReason resp
                     case findHeader HdrContentType resp of
                       Just "text/plain" -> notice verbosity $ rspBody resp
                       _                 -> debug verbosity $ rspBody resp

mkRequest :: URI -> FilePath -> IO Request
mkRequest uri path = 
    do pkg <- readBinaryFile path
       boundary <- genBoundary
       let body = printMultiPart boundary (mkFormData path pkg)
       return $ Request {
                         rqURI = uri,
                         rqMethod = POST,
                         rqHeaders = [Header HdrContentType ("multipart/form-data; boundary="++boundary),
                                      Header HdrContentLength (show (length body)),
                                      Header HdrAccept ("text/plain")],
                         rqBody = body
                        }

readBinaryFile :: FilePath -> IO String
readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents

genBoundary :: IO String
genBoundary = do i <- randomRIO (0x10000000000000,0xFFFFFFFFFFFFFF) :: IO Integer
                 return $ showHex i ""

mkFormData :: FilePath -> String -> [BodyPart]
mkFormData path pkg = 
    -- yes, web browsers are that stupid (re quoting)
    [BodyPart [Header hdrContentDisposition ("form-data; name=package; filename=\""++path++"\""),
               Header HdrContentType "application/x-gzip"] 
     pkg]

hdrContentDisposition :: HeaderName
hdrContentDisposition = HdrCustom "Content-disposition"

-- * Multipart, partly stolen from the cgi package.

data BodyPart = BodyPart [Header] String

printMultiPart :: String -> [BodyPart] -> String
printMultiPart boundary xs = 
    concatMap (printBodyPart boundary) xs ++ crlf ++ "--" ++ boundary ++ "--" ++ crlf

printBodyPart :: String -> BodyPart -> String
printBodyPart boundary (BodyPart hs c) = crlf ++ "--" ++ boundary ++ crlf ++ concatMap show hs ++ crlf ++ c

crlf :: String
crlf = "\r\n"

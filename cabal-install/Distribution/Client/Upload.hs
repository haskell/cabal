-- This is a quick hack for uploading packages to Hackage.
-- See http://hackage.haskell.org/trac/hackage/wiki/CabalUpload

module Distribution.Client.Upload (check, upload, report) where

import Distribution.Client.Types (Username(..), Password(..),Repo(..),RemoteRepo(..))
import Distribution.Client.HttpUtils
         ( isOldHackageURI, HttpTransport(..), remoteRepoTryUpgradeToHttps )

import Distribution.Simple.Utils (notice, warn, info, die)
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Client.Config

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import qualified Distribution.Client.BuildReports.Upload as BuildReport

import Network.URI (URI(uriPath), parseURI)

import System.IO        (hFlush, stdin, stdout, hGetEcho, hSetEcho)
import Control.Exception (bracket)
import System.FilePath  ((</>), takeExtension)
import qualified System.FilePath.Posix as FilePath.Posix ((</>))
import System.Directory
import Control.Monad (forM_, when)

type Auth = Maybe (String, String)

--FIXME: how do we find this path for an arbitrary hackage server?
-- is it always at some fixed location relative to the server root?
legacyUploadURI :: URI
Just legacyUploadURI = parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/protected/upload-pkg"

checkURI :: URI
Just checkURI = parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/check-pkg"

upload :: HttpTransport -> Verbosity -> [Repo] -> Maybe Username -> Maybe Password -> [FilePath] -> IO ()
upload transport verbosity repos mUsername mPassword paths = do
    targetRepo <-
      case [ remoteRepo | Left remoteRepo <- map repoKind repos ] of
        [] -> die $ "Cannot upload. No remote repositories are configured."
        rs -> remoteRepoTryUpgradeToHttps transport (last rs)
    let targetRepoURI = remoteRepoURI targetRepo
        uploadURI
          | isOldHackageURI targetRepoURI
          = legacyUploadURI
          | otherwise
          = targetRepoURI {
              uriPath = uriPath targetRepoURI FilePath.Posix.</> "upload"
            }
    Username username <- maybe promptUsername return mUsername
    Password password <- maybe promptPassword return mPassword
    let auth = Just (username,password)
    flip mapM_ paths $ \path -> do
      notice verbosity $ "Uploading " ++ path ++ "... "
      handlePackage transport verbosity uploadURI auth path

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
  passwd <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False  -- no echoing for entering the password
    fmap Password getLine
  putStrLn ""
  return passwd

report :: Verbosity -> [Repo] -> Maybe Username -> Maybe Password -> IO ()
report verbosity repos mUsername mPassword = do
      Username username <- maybe promptUsername return mUsername
      Password password <- maybe promptPassword return mPassword
      let auth = (username,password)
      forM_ repos $ \repo -> case repoKind repo of
        Left remoteRepo
            -> do dotCabal <- defaultCabalDir
                  let srcDir = dotCabal </> "reports" </> remoteRepoName remoteRepo
                  -- We don't want to bomb out just because we haven't built any packages from this repo yet
                  srcExists <- doesDirectoryExist srcDir
                  when srcExists $ do
                    contents <- getDirectoryContents srcDir
                    forM_ (filter (\c -> takeExtension c == ".log") contents) $ \logFile ->
                        do inp <- readFile (srcDir </> logFile)
                           let (reportStr, buildLog) = read inp :: (String,String)
                           case BuildReport.parse reportStr of
                             Left errs -> do warn verbosity $ "Errors: " ++ errs -- FIXME
                             Right report' ->
                                 do info verbosity $ "Uploading report for " ++ display (BuildReport.package report')
                                    BuildReport.uploadReports verbosity auth (remoteRepoURI remoteRepo) [(report', Just buildLog)]
                                    return ()
        Right{} -> return ()

check :: HttpTransport -> Verbosity -> [FilePath] -> IO ()
check transport verbosity paths = do
          flip mapM_ paths $ \path -> do
            notice verbosity $ "Checking " ++ path ++ "... "
            handlePackage transport verbosity checkURI Nothing path

handlePackage :: HttpTransport -> Verbosity -> URI -> Auth
              -> FilePath -> IO ()
handlePackage transport verbosity uri auth path =
  do resp <- postHttpFile transport verbosity uri path auth
     case resp of
       (200,_)     -> do notice verbosity "Ok"
       (code,err)  -> do notice verbosity $ "Error uploading " ++ path ++ ": "
                                     ++ "http code " ++ show code ++ "\n"
                                     ++ err


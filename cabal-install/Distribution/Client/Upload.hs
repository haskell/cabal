module Distribution.Client.Upload (check, upload, uploadDoc, report) where

import Distribution.Client.Types ( Username(..), Password(..)
                                 , RemoteRepo(..), maybeRepoRemote )
import Distribution.Client.HttpUtils
         ( HttpTransport(..), remoteRepoTryUpgradeToHttps )
import Distribution.Client.Setup
         ( RepoContext(..) )

import Distribution.Simple.Utils (notice, warn, info, die)
import Distribution.Verbosity (Verbosity)
import Distribution.Text (display)
import Distribution.Client.Config

import qualified Distribution.Client.BuildReports.Anonymous as BuildReport
import qualified Distribution.Client.BuildReports.Upload as BuildReport

import Network.URI (URI(uriPath), parseURI)
import Network.HTTP (Header(..), HeaderName(..))

import System.IO        (hFlush, stdin, stdout, hGetEcho, hSetEcho)
import System.Exit      (exitFailure)
import Control.Exception (bracket)
import System.FilePath  ((</>), takeExtension, takeFileName)
import qualified System.FilePath.Posix as FilePath.Posix ((</>))
import System.Directory
import Control.Monad (forM_, when)
import Data.Maybe (catMaybes)

type Auth = Maybe (String, String)

checkURI :: URI
Just checkURI = parseURI $ "http://hackage.haskell.org/cgi-bin/"
                           ++ "hackage-scripts/check-pkg"

upload :: Verbosity -> RepoContext
       -> Maybe Username -> Maybe Password -> [FilePath]
       -> IO ()
upload verbosity repoCtxt mUsername mPassword paths = do
    let repos = repoContextRepos repoCtxt
    transport  <- repoContextGetTransport repoCtxt
    targetRepo <-
      case [ remoteRepo | Just remoteRepo <- map maybeRepoRemote repos ] of
        [] -> die "Cannot upload. No remote repositories are configured."
        rs -> remoteRepoTryUpgradeToHttps transport (last rs)
    let targetRepoURI = remoteRepoURI targetRepo
        rootIfEmpty x = if null x then "/" else x
        uploadURI = targetRepoURI {
            uriPath = rootIfEmpty (uriPath targetRepoURI)
                      FilePath.Posix.</> "upload"
        }
    Username username <- maybe promptUsername return mUsername
    Password password <- maybe promptPassword return mPassword
    let auth = Just (username,password)
    forM_ paths $ \path -> do
      notice verbosity $ "Uploading " ++ path ++ "... "
      handlePackage transport verbosity uploadURI auth path

uploadDoc :: Verbosity -> RepoContext
          -> Maybe Username -> Maybe Password -> FilePath
          -> IO ()
uploadDoc verbosity repoCtxt mUsername mPassword path = do
    let repos = repoContextRepos repoCtxt
    transport  <- repoContextGetTransport repoCtxt
    targetRepo <-
      case [ remoteRepo | Just remoteRepo <- map maybeRepoRemote repos ] of
        [] -> die $ "Cannot upload. No remote repositories are configured."
        rs -> remoteRepoTryUpgradeToHttps transport (last rs)
    let targetRepoURI = remoteRepoURI targetRepo
        rootIfEmpty x = if null x then "/" else x
        uploadURI = targetRepoURI {
            uriPath = rootIfEmpty (uriPath targetRepoURI)
                      FilePath.Posix.</> "package/" ++ pkgid ++ "/docs"
        }
        (reverseSuffix, reversePkgid) = break (== '-')
                                        (reverse (takeFileName path))
        pkgid = reverse $ tail reversePkgid
    when (reverse reverseSuffix /= "docs.tar.gz"
          || null reversePkgid || head reversePkgid /= '-') $
      die "Expected a file name matching the pattern <pkgid>-docs.tar.gz"
    Username username <- maybe promptUsername return mUsername
    Password password <- maybe promptPassword return mPassword

    let auth = Just (username,password)
        headers =
          [ Header HdrContentType "application/x-tar"
          , Header HdrContentEncoding "gzip"
          ]
    notice verbosity $ "Uploading documentation " ++ path ++ "... "
    resp <- putHttpFile transport verbosity uploadURI path auth headers
    case resp of
      (200,_)     ->
        notice verbosity "Ok"
      (code,err)  -> do
        notice verbosity $ "Error uploading documentation "
                        ++ path ++ ": "
                        ++ "http code " ++ show code ++ "\n"
                        ++ err
        exitFailure

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

report :: Verbosity -> RepoContext -> Maybe Username -> Maybe Password -> IO ()
report verbosity repoCtxt mUsername mPassword = do
  Username username <- maybe promptUsername return mUsername
  Password password <- maybe promptPassword return mPassword
  let auth        = (username, password)
      repos       = repoContextRepos repoCtxt
      remoteRepos = catMaybes (map maybeRepoRemote repos)
  forM_ remoteRepos $ \remoteRepo ->
      do dotCabal <- defaultCabalDir
         let srcDir = dotCabal </> "reports" </> remoteRepoName remoteRepo
         -- We don't want to bomb out just because we haven't built any packages
         -- from this repo yet.
         srcExists <- doesDirectoryExist srcDir
         when srcExists $ do
           contents <- getDirectoryContents srcDir
           forM_ (filter (\c -> takeExtension c ==".log") contents) $ \logFile ->
             do inp <- readFile (srcDir </> logFile)
                let (reportStr, buildLog) = read inp :: (String,String)
                case BuildReport.parse reportStr of
                  Left errs -> warn verbosity $ "Errors: " ++ errs -- FIXME
                  Right report' ->
                    do info verbosity $ "Uploading report for "
                         ++ display (BuildReport.package report')
                       BuildReport.uploadReports verbosity repoCtxt auth
                         (remoteRepoURI remoteRepo) [(report', Just buildLog)]
                       return ()

check :: Verbosity -> RepoContext -> [FilePath] -> IO ()
check verbosity repoCtxt paths = do
    transport <- repoContextGetTransport repoCtxt
    forM_ paths $ \path -> do
      notice verbosity $ "Checking " ++ path ++ "... "
      handlePackage transport verbosity checkURI Nothing path

handlePackage :: HttpTransport -> Verbosity -> URI -> Auth
              -> FilePath -> IO ()
handlePackage transport verbosity uri auth path =
  do resp <- postHttpFile transport verbosity uri path auth
     case resp of
       (200,_)     ->
          notice verbosity "Ok"
       (code,err)  -> do
          notice verbosity $ "Error uploading " ++ path ++ ": "
                          ++ "http code " ++ show code ++ "\n"
                          ++ err
          exitFailure

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Distribution.Client.FetchUtils
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for fetching packages
module Distribution.Client.FetchUtils
  ( -- * fetching packages
    fetchPackage
  , isFetched
  , checkFetched

    -- ** specifically for repo packages
  , checkRepoTarballFetched
  , fetchRepoTarball
  , verifyFetchedTarball

    -- ** fetching packages asynchronously
  , asyncFetchPackages
  , waitAsyncFetchPackage
  , AsyncFetchMap

    -- * fetching other things
  , downloadIndex
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.HttpUtils
  ( DownloadResult (..)
  , HttpTransport (..)
  , downloadURI
  , isOldHackageURI
  , remoteRepoCheckHttps
  , transportCheckHttps
  )
import Distribution.Client.Types

import Distribution.Client.GlobalFlags
  ( RepoContext (..)
  )
import Distribution.Client.Utils
  ( ProgressPhase (..)
  , progressMessage
  )
import Distribution.Package
  ( PackageId
  , packageName
  , packageVersion
  )
import Distribution.Simple.Utils
  ( debug
  , dieWithException
  , info
  , notice
  , warn
  )
import Distribution.Verbosity
  ( verboseUnmarkOutput
  )

import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Control.Exception.Safe as Safe
import qualified Data.Map as Map
import Network.URI
  ( URI (uriPath)
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getFileSize
  , getTemporaryDirectory
  )
import System.FilePath
  ( (<.>)
  , (</>)
  )
import qualified System.FilePath.Posix as FilePath.Posix
  ( combine
  , joinPath
  )
import System.IO
  ( hClose
  , openTempFile
  )

import Distribution.Client.Errors
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Util.Checked as Sec
import qualified Hackage.Security.Util.Path as Sec

-- ------------------------------------------------------------

-- * Actually fetch things

-- ------------------------------------------------------------

-- | Returns @True@ if the package has already been fetched
-- or does not need fetching.
isFetched :: UnresolvedPkgLoc -> IO Bool
isFetched loc = case loc of
  LocalUnpackedPackage _dir -> return True
  LocalTarballPackage _file -> return True
  RemoteTarballPackage _uri local -> return (isJust local)
  RepoTarballPackage repo pkgid _ -> doesFileExist (packageFile repo pkgid)
  RemoteSourceRepoPackage _ local -> return (isJust local)

-- | Checks if the package has already been fetched (or does not need
-- fetching) and if so returns evidence in the form of a 'PackageLocation'
-- with a resolved local file location.
checkFetched
  :: UnresolvedPkgLoc
  -> IO (Maybe ResolvedPkgLoc)
checkFetched loc = case loc of
  LocalUnpackedPackage dir ->
    return (Just $ LocalUnpackedPackage dir)
  LocalTarballPackage file ->
    return (Just $ LocalTarballPackage file)
  RemoteTarballPackage uri (Just file) ->
    return (Just $ RemoteTarballPackage uri file)
  RepoTarballPackage repo pkgid (Just file) ->
    return (Just $ RepoTarballPackage repo pkgid file)
  RemoteSourceRepoPackage repo (Just file) ->
    return (Just $ RemoteSourceRepoPackage repo file)
  RemoteTarballPackage _uri Nothing -> return Nothing
  RemoteSourceRepoPackage _repo Nothing -> return Nothing
  RepoTarballPackage repo pkgid Nothing ->
    fmap
      (fmap (RepoTarballPackage repo pkgid))
      (checkRepoTarballFetched repo pkgid)

-- | Like 'checkFetched' but for the specific case of a 'RepoTarballPackage'.
checkRepoTarballFetched :: Repo -> PackageId -> IO (Maybe FilePath)
checkRepoTarballFetched repo pkgid = do
  let file = packageFile repo pkgid
  exists <- doesFileExist file
  if exists
    then return (Just file)
    else return Nothing

verifyFetchedTarball :: Verbosity -> RepoContext -> Repo -> PackageId -> IO Bool
verifyFetchedTarball verbosity repoCtxt repo pkgid =
  let file = packageFile repo pkgid
      handleError :: IO Bool -> IO Bool
      handleError act = do
        res <- Safe.try act
        case res of
          Left e -> warn verbosity ("Error verifying fetched tarball " ++ file ++ ", will redownload: " ++ show (e :: SomeException)) >> pure False
          Right b -> pure b
   in handleError $ do
        exists <- doesFileExist file
        if not exists
          then return True -- if the file does not exist, it vacuously passes validation, since it will be downloaded as necessary with what we will then check is a valid hash.
          else case repo of
            -- a secure repo has hashes we can compare against to confirm this is the correct file.
            RepoSecure{} ->
              repoContextWithSecureRepo repoCtxt repo $ \repoSecure ->
                Sec.withIndex repoSecure $ \callbacks ->
                  let warnAndFail s = warn verbosity ("Fetched tarball " ++ file ++ " does not match server, will redownload: " ++ s) >> return False
                   in -- the do block in parens is due to dealing with the checked exceptions mechanism.
                      ( do
                          fileInfo <- Sec.indexLookupFileInfo callbacks pkgid
                          sz <- Sec.FileLength . fromInteger <$> getFileSize file
                          if sz /= Sec.fileInfoLength (Sec.trusted fileInfo)
                            then warnAndFail "file length mismatch"
                            else do
                              res <- Sec.compareTrustedFileInfo (Sec.trusted fileInfo) <$> Sec.computeFileInfo (Sec.Path file :: Sec.Path Sec.Absolute)
                              if res
                                then pure True
                                else warnAndFail "file hash mismatch"
                      )
                        `Sec.catchChecked` (\(e :: Sec.InvalidPackageException) -> warnAndFail (show e))
                        `Sec.catchChecked` (\(e :: Sec.VerificationError) -> warnAndFail (show e))
            _ -> pure True

-- | Fetch a package if we don't have it already.
fetchPackage
  :: Verbosity
  -> RepoContext
  -> UnresolvedPkgLoc
  -> IO ResolvedPkgLoc
fetchPackage verbosity repoCtxt loc = case loc of
  LocalUnpackedPackage dir ->
    return (LocalUnpackedPackage dir)
  LocalTarballPackage file ->
    return (LocalTarballPackage file)
  RemoteTarballPackage uri (Just file) ->
    return (RemoteTarballPackage uri file)
  RepoTarballPackage repo pkgid (Just file) ->
    return (RepoTarballPackage repo pkgid file)
  RemoteSourceRepoPackage repo (Just dir) ->
    return (RemoteSourceRepoPackage repo dir)
  RemoteTarballPackage uri Nothing -> do
    path <- downloadTarballPackage uri
    return (RemoteTarballPackage uri path)
  RepoTarballPackage repo pkgid Nothing -> do
    local <- fetchRepoTarball verbosity repoCtxt repo pkgid
    return (RepoTarballPackage repo pkgid local)
  RemoteSourceRepoPackage _repo Nothing ->
    dieWithException verbosity FetchPackageErr
  where
    downloadTarballPackage :: URI -> IO FilePath
    downloadTarballPackage uri = do
      transport <- repoContextGetTransport repoCtxt
      transportCheckHttps verbosity transport uri
      notice verbosity ("Downloading " ++ show uri)
      tmpdir <- getTemporaryDirectory
      (path, hnd) <- openTempFile tmpdir "cabal-.tar.gz"
      hClose hnd
      _ <- downloadURI transport verbosity uri path
      return path

-- | Fetch a repo package if we don't have it already.
fetchRepoTarball :: Verbosity -> RepoContext -> Repo -> PackageId -> IO FilePath
fetchRepoTarball verbosity' repoCtxt repo pkgid = do
  fetched <- doesFileExist (packageFile repo pkgid)
  if fetched
    then do
      info verbosity $ prettyShow pkgid ++ " has already been downloaded."
      return (packageFile repo pkgid)
    else do
      progressMessage verbosity ProgressDownloading (prettyShow pkgid)
      res <- downloadRepoPackage
      progressMessage verbosity ProgressDownloaded (prettyShow pkgid)
      return res
  where
    -- whether we download or not is non-deterministic
    verbosity = verboseUnmarkOutput verbosity'

    downloadRepoPackage :: IO FilePath
    downloadRepoPackage = case repo of
      RepoLocalNoIndex{} -> return (packageFile repo pkgid)
      RepoRemote{..} -> do
        transport <- repoContextGetTransport repoCtxt
        remoteRepoCheckHttps verbosity transport repoRemote
        let uri = packageURI repoRemote pkgid
            dir = packageDir repo pkgid
            path = packageFile repo pkgid
        createDirectoryIfMissing True dir
        _ <- downloadURI transport verbosity uri path
        return path
      RepoSecure{} -> repoContextWithSecureRepo repoCtxt repo $ \rep -> do
        let dir = packageDir repo pkgid
            path = packageFile repo pkgid
        createDirectoryIfMissing True dir
        Sec.uncheckClientErrors $ do
          info verbosity ("Writing " ++ path)
          Sec.downloadPackage' rep pkgid path
        return path

-- | Downloads an index file to [config-dir/packages/serv-id] without
-- hackage-security. You probably don't want to call this directly;
-- use 'updateRepo' instead.
downloadIndex :: HttpTransport -> Verbosity -> RemoteRepo -> FilePath -> IO DownloadResult
downloadIndex transport verbosity remoteRepo cacheDir = do
  remoteRepoCheckHttps verbosity transport remoteRepo
  let uri =
        (remoteRepoURI remoteRepo)
          { uriPath =
              uriPath (remoteRepoURI remoteRepo)
                `FilePath.Posix.combine` "00-index.tar.gz"
          }
      path = cacheDir </> "00-index" <.> "tar.gz"
  createDirectoryIfMissing True cacheDir
  downloadURI transport verbosity uri path

-- ------------------------------------------------------------

-- * Async fetch wrapper utilities

-- ------------------------------------------------------------

type AsyncFetchMap =
  Map
    UnresolvedPkgLoc
    (MVar (Either SomeException ResolvedPkgLoc))

-- | Fork off an async action to download the given packages (by location).
--
-- The downloads are initiated in order, so you can arrange for packages that
-- will likely be needed sooner to be earlier in the list.
--
-- The body action is passed a map from those packages (identified by their
-- location) to a completion var for that package. So the body action should
-- lookup the location and use 'waitAsyncFetchPackage' to get the result.
--
-- Synchronous exceptions raised by the download actions are delivered
-- via 'waitAsyncFetchPackage'.
asyncFetchPackages
  :: Verbosity
  -> RepoContext
  -> [UnresolvedPkgLoc]
  -> (AsyncFetchMap -> IO a)
  -> IO a
asyncFetchPackages verbosity repoCtxt pkglocs body = do
  -- TODO: [nice to have] use parallel downloads?

  asyncDownloadVars <-
    sequenceA
      [ do
        v <- newEmptyMVar
        return (pkgloc, v)
      | pkgloc <- pkglocs
      ]

  let fetchPackages :: IO ()
      fetchPackages =
        for_ asyncDownloadVars $ \(pkgloc, var) -> do
          -- Suppress marking here, because 'withAsync' means
          -- that we get nondeterministic interleaving.
          -- It is essential that we don't catch async exceptions here,
          -- specifically 'AsyncCancelled' thrown at us from 'concurrently'.
          result <-
            Safe.try $
              fetchPackage (verboseUnmarkOutput verbosity) repoCtxt pkgloc
          putMVar var result

  (_, res) <-
    concurrently
      fetchPackages
      (body $ Map.fromList asyncDownloadVars)
  pure res

-- | Expect to find a download in progress in the given 'AsyncFetchMap'
-- and wait on it to finish.
--
-- If the download failed with an exception then this will be thrown.
--
-- Note: This function is supposed to be idempotent, as our install plans
-- can now use the same tarball for many builds, e.g. different
-- components and/or qualified goals, and these all go through the
-- download phase so we end up using 'waitAsyncFetchPackage' twice on
-- the same package. C.f. #4461.
waitAsyncFetchPackage
  :: Verbosity
  -> AsyncFetchMap
  -> UnresolvedPkgLoc
  -> IO ResolvedPkgLoc
waitAsyncFetchPackage verbosity downloadMap srcloc =
  case Map.lookup srcloc downloadMap of
    Just hnd -> do
      debug verbosity $ "Waiting for download of " ++ show srcloc
      either throwIO return =<< readMVar hnd
    Nothing -> fail "waitAsyncFetchPackage: package not being downloaded"

-- ------------------------------------------------------------

-- * Path utilities

-- ------------------------------------------------------------

-- | Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifier@.
packageFile :: Repo -> PackageId -> FilePath
packageFile repo pkgid =
  packageDir repo pkgid
    </> prettyShow pkgid
    <.> "tar.gz"

-- | Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifier@ is stored.
packageDir :: Repo -> PackageId -> FilePath
packageDir (RepoLocalNoIndex (LocalRepo _ dir _) _) _pkgid = dir
packageDir repo pkgid =
  repoLocalDir repo
    </> prettyShow (packageName pkgid)
    </> prettyShow (packageVersion pkgid)

-- | Generate the URI of the tarball for a given package.
packageURI :: RemoteRepo -> PackageId -> URI
packageURI repo pkgid
  | isOldHackageURI (remoteRepoURI repo) =
      (remoteRepoURI repo)
        { uriPath =
            FilePath.Posix.joinPath
              [ uriPath (remoteRepoURI repo)
              , prettyShow (packageName pkgid)
              , prettyShow (packageVersion pkgid)
              , prettyShow pkgid <.> "tar.gz"
              ]
        }
packageURI repo pkgid =
  (remoteRepoURI repo)
    { uriPath =
        FilePath.Posix.joinPath
          [ uriPath (remoteRepoURI repo)
          , "package"
          , prettyShow pkgid <.> "tar.gz"
          ]
    }

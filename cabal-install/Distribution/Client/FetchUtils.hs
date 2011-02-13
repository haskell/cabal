-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
module Distribution.Client.FetchUtils (

    -- * fetching packages
    isFetched,

    -- ** specifically for repo packages
    fetchRepoTarball,

    -- * fetching other things
    downloadIndex,
  ) where

import Distribution.Client.Types
import Distribution.Client.HttpUtils
         ( downloadURI, isOldHackageURI )

import Distribution.Package
         ( PackageId, packageName, packageVersion )
import Distribution.Simple.Utils
         ( info, debug, setupMessage )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import System.Directory
         ( doesFileExist, createDirectoryIfMissing )
import System.FilePath
         ( (</>), (<.>) )
import qualified System.FilePath.Posix as FilePath.Posix
         ( combine, joinPath )
import Network.URI
         ( URI(uriPath) )

-- ------------------------------------------------------------
-- * Actually fetch things
-- ------------------------------------------------------------

-- | Returns @True@ if the package has already been fetched
-- or does not need fetching.
--
isFetched :: AvailablePackage -> IO Bool
isFetched (AvailablePackage pkgid _ source) = case source of
  LocalUnpackedPackage _  -> return True
  LocalTarballPackage  _  -> return True
  RemoteTarballPackage _  -> return False --TODO: ad-hoc download caching
  RepoTarballPackage repo -> doesFileExist (packageFile repo pkgid)

-- | Fetch a repo package if we don't have it already.
--
fetchRepoTarball :: Verbosity -> Repo -> PackageId -> IO FilePath
fetchRepoTarball verbosity repo pkgid = do
  fetched <- doesFileExist (packageFile repo pkgid)
  if fetched
    then do info verbosity $ display pkgid ++ " has already been downloaded."
            return (packageFile repo pkgid)
    else do setupMessage verbosity "Downloading" pkgid
            downloadPackage verbosity repo pkgid

-- Downloads a package to [config-dir/packages/package-id] and returns the path to the package.
downloadPackage :: Verbosity -> Repo -> PackageId -> IO String
downloadPackage _ repo@Repo{ repoKind = Right LocalRepo } pkgid =
  return (packageFile repo pkgid)

downloadPackage verbosity repo@Repo{ repoKind = Left remoteRepo } pkgid = do
  let uri  = packageURI remoteRepo pkgid
      dir  = packageDir       repo pkgid
      path = packageFile      repo pkgid
  debug verbosity $ "GET " ++ show uri
  createDirectoryIfMissing True dir
  downloadURI verbosity uri path
  return path

-- | Downloads an index file to [config-dir/packages/serv-id].
--
downloadIndex :: Verbosity -> RemoteRepo -> FilePath -> IO FilePath
downloadIndex verbosity repo cacheDir = do
  let uri = (remoteRepoURI repo) {
              uriPath = uriPath (remoteRepoURI repo)
                          `FilePath.Posix.combine` "00-index.tar.gz"
            }
      path = cacheDir </> "00-index" <.> "tar.gz"
  createDirectoryIfMissing True cacheDir
  downloadURI verbosity uri path
  return path


-- ------------------------------------------------------------
-- * Path utilities
-- ------------------------------------------------------------

-- | Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
--
packageFile :: Repo -> PackageId -> FilePath
packageFile repo pkgid = packageDir repo pkgid
                     </> display pkgid
                     <.> "tar.gz"

-- | Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
--
packageDir :: Repo -> PackageId -> FilePath
packageDir repo pkgid = repoLocalDir repo
                    </> display (packageName    pkgid)
                    </> display (packageVersion pkgid)

-- | Generate the URI of the tarball for a given package.
--
packageURI :: RemoteRepo -> PackageId -> URI
packageURI repo pkgid | isOldHackageURI (remoteRepoURI repo) =
  (remoteRepoURI repo) {
    uriPath = FilePath.Posix.joinPath
      [uriPath (remoteRepoURI repo)
      ,display (packageName    pkgid)
      ,display (packageVersion pkgid)
      ,display pkgid <.> "tar.gz"]
  }
packageURI repo pkgid =
  (remoteRepoURI repo) {
    uriPath = FilePath.Posix.joinPath
      [uriPath (remoteRepoURI repo)
      ,"package"
      ,display pkgid <.> "tar.gz"]
  }

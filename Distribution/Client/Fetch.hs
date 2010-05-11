-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Fetch
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Fetch (

    -- * Commands
    fetch,

    -- * Utilities
    fetchPackage,
    isFetched,
    downloadIndex,
  ) where

import Distribution.Client.Types
         ( UnresolvedDependency (..), AvailablePackage(..)
         , AvailablePackageSource(..), AvailablePackageDb(..)
         , Repo(..), RemoteRepo(..), LocalRepo(..) )
import Distribution.Client.Dependency
         ( resolveDependenciesWithProgress
         , dependencyConstraints, dependencyTargets
         , PackagesPreference(..), PackagesPreferenceDefault(..)
         , PackagePreference(..) )
import Distribution.Client.Dependency.Types
         ( foldProgress )
import Distribution.Client.IndexUtils as IndexUtils
         ( getAvailablePackages, disambiguateDependencies
         , getInstalledPackages )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.HttpUtils
         ( downloadURI, isOldHackageURI )

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, Dependency(..) )
import qualified Distribution.Client.PackageIndex as PackageIndex
import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Utils
         ( die, notice, info, debug, setupMessage )
import Distribution.System
         ( buildPlatform )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import qualified Data.Map as Map
import Control.Monad
         ( when, filterM )
import System.Directory
         ( doesFileExist, createDirectoryIfMissing )
import System.FilePath
         ( (</>), (<.>) )
import qualified System.FilePath.Posix as FilePath.Posix
         ( combine, joinPath )
import Network.URI
         ( URI(uriPath) )


-- Downloads a package to [config-dir/packages/package-id] and returns the path to the package.
downloadPackage :: Verbosity -> Repo -> PackageIdentifier -> IO String
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

-- Downloads an index file to [config-dir/packages/serv-id].
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

-- |Returns @True@ if the package has already been fetched.
isFetched :: AvailablePackage -> IO Bool
isFetched (AvailablePackage pkgid _ source) = case source of
  LocalUnpackedPackage _  -> return True
  LocalTarballPackage  _  -> return True
  RemoteTarballPackage _  -> return False --TODO: ad-hoc download caching
  RepoTarballPackage repo -> doesFileExist (packageFile repo pkgid)

-- |Fetch a package if we don't have it already.
fetchPackage :: Verbosity -> Repo -> PackageIdentifier -> IO String
fetchPackage verbosity repo pkgid = do
  fetched <- doesFileExist (packageFile repo pkgid)
  if fetched
    then do info verbosity $ display pkgid ++ " has already been downloaded."
            return (packageFile repo pkgid)
    else do setupMessage verbosity "Downloading" pkgid
            downloadPackage verbosity repo pkgid

-- |Fetch a list of packages and their dependencies.
fetch :: Verbosity
      -> PackageDBStack
      -> [Repo]
      -> Compiler
      -> ProgramConfiguration
      -> [UnresolvedDependency]
      -> IO ()
fetch verbosity packageDBs repos comp conf deps = do
  installed <- getInstalledPackages verbosity comp packageDBs conf
  AvailablePackageDb available availablePrefs
            <- getAvailablePackages verbosity repos
  deps' <- IndexUtils.disambiguateDependencies available deps

  let -- Hide the packages given on the command line so that the dep resolver
      -- will decide that they need fetching, even if they're already
      -- installed. Sicne we want to get the source packages of things we might
      -- have installed (but not have the sources for).
      installed' = hideGivenDeps deps' installed
      hideGivenDeps pkgs index =
        foldr PackageIndex.deletePackageName index
          [ name | UnresolvedDependency (Dependency name _) _ <- pkgs ]

  let  progress = resolveDependenciesWithProgress
                   buildPlatform (compilerId comp)
                   installed' available
                   (PackagesPreference PreferLatestForSelected
                     [ PackageVersionPreference name ver
                     | (name, ver) <- Map.toList availablePrefs ])
                   (dependencyConstraints deps')
                   (dependencyTargets deps')
  notice verbosity "Resolving dependencies..."
  maybePlan <- foldProgress (\message rest -> info verbosity message >> rest)
                            (return . Left) (return . Right) progress
  case maybePlan of
    Left message -> die message
    Right pkgs   -> do
      ps <- filterM (fmap not . isFetched)
              [ pkg | (InstallPlan.Configured
                        (InstallPlan.ConfiguredPackage pkg _ _))
                          <- InstallPlan.toList pkgs ]
      when (null ps) $
        notice verbosity $ "No packages need to be fetched. "
                        ++ "All the requested packages are already cached."

      sequence_ 
        [ fetchPackage verbosity repo pkgid
        | (AvailablePackage pkgid _ (RepoTarballPackage repo)) <- ps ]

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: Repo -> PackageIdentifier -> FilePath
packageFile repo pkgid = packageDir repo pkgid
                     </> display pkgid
                     <.> "tar.gz"

-- |Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
packageDir :: Repo -> PackageIdentifier -> FilePath
packageDir repo pkgid = repoLocalDir repo
                    </> display (packageName    pkgid)
                    </> display (packageVersion pkgid)

-- | Generate the URI of the tarball for a given package.
packageURI :: RemoteRepo -> PackageIdentifier -> URI
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

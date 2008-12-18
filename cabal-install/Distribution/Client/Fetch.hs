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
         ( getAvailablePackages, disambiguateDependencies )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.HttpUtils (getHTTP, isOldHackageURI)

import Distribution.Package
         ( PackageIdentifier, packageName, packageVersion, Dependency(..) )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Compiler
         ( Compiler(compilerId), PackageDB )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import Distribution.Simple.Configure
         ( getInstalledPackages )
import Distribution.Simple.Utils
         ( die, notice, info, debug, setupMessage
         , copyFileVerbose, writeFileAtomic )
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
         ( URI(uriPath, uriScheme) )
import Network.HTTP
         ( ConnError(..), Response(..) )


downloadURI :: Verbosity
            -> FilePath -- ^ Where to put it
            -> URI      -- ^ What to download
            -> IO (Maybe ConnError)
downloadURI verbosity path uri | uriScheme uri == "file:" = do
  copyFileVerbose verbosity (uriPath uri) path
  return Nothing
downloadURI verbosity path uri = do
  eitherResult <- getHTTP verbosity uri
  case eitherResult of
    Left err -> return (Just err)
    Right rsp
      | rspCode rsp == (2,0,0)
     -> do info verbosity ("Downloaded to " ++ path)
           writeFileAtomic path (rspBody rsp)
     --FIXME: check the content-length header matches the body length.
     --TODO: stream the download into the file rather than buffering the whole
     --      thing in memory.
     --      remember the ETag so we can not re-download if nothing changed.
     >> return Nothing

      | otherwise
     -> return (Just (ErrorMisc ("Unsucessful HTTP code: " ++ show (rspCode rsp))))

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
  status <- downloadURI verbosity path uri
  case status of
    Just err -> die $ "Failed to download '" ++ display pkgid
                   ++ "': " ++ show err
    Nothing  -> return path

-- Downloads an index file to [config-dir/packages/serv-id].
downloadIndex :: Verbosity -> RemoteRepo -> FilePath -> IO FilePath
downloadIndex verbosity repo cacheDir = do
  let uri = (remoteRepoURI repo) {
              uriPath = uriPath (remoteRepoURI repo)
                          `FilePath.Posix.combine` "00-index.tar.gz"
            }
      path = cacheDir </> "00-index" <.> "tar.gz"
  createDirectoryIfMissing True cacheDir
  mbError <- downloadURI verbosity path uri
  case mbError of
    Just err -> die $ "Failed to download index '" ++ show err ++ "'"
    Nothing  -> return path

-- |Returns @True@ if the package has already been fetched.
isFetched :: AvailablePackage -> IO Bool
isFetched (AvailablePackage pkgid _ source) = case source of
  LocalUnpackedPackage    -> return True
  RepoTarballPackage repo -> doesFileExist (packageFile repo pkgid)

-- |Fetch a package if we don't have it already.
fetchPackage :: Verbosity -> Repo -> PackageIdentifier -> IO String
fetchPackage verbosity repo pkgid = do
  fetched <- doesFileExist (packageFile repo pkgid)
  if fetched
    then do notice verbosity $ "'" ++ display pkgid ++ "' is cached."
            return (packageFile repo pkgid)
    else do setupMessage verbosity "Downloading" pkgid
            downloadPackage verbosity repo pkgid

-- |Fetch a list of packages and their dependencies.
fetch :: Verbosity
      -> PackageDB
      -> [Repo]
      -> Compiler
      -> ProgramConfiguration
      -> [UnresolvedDependency]
      -> IO ()
fetch verbosity packageDB repos comp conf deps = do
  installed <- getInstalledPackages verbosity comp packageDB conf
  AvailablePackageDb available availablePrefs
            <- getAvailablePackages verbosity repos
  deps' <- IndexUtils.disambiguateDependencies available deps

  let -- Hide the packages given on the command line so that the dep resolver
      -- will decide that they need fetching, even if they're already
      -- installed. Sicne we want to get the source packages of things we might
      -- have installed (but not have the sources for).
      installed' = fmap (hideGivenDeps deps') installed
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
      ,"packages"
      ,display pkgid
      ,"tarball"]
  }

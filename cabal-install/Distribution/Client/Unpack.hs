-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Unpack
-- Copyright   :  (c) Andrea Vezzosi 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Distribution.Client.Unpack (

    -- * Commands
    unpack,

  ) where

import Distribution.Package
         ( PackageId, Dependency(..) )
import Distribution.Simple.Setup(fromFlag, fromFlagOrDefault)
import Distribution.Simple.Utils
         ( notice, die )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text(display)

import Distribution.Client.Setup(UnpackFlags(unpackVerbosity,
                                             unpackDestDir))
import Distribution.Client.Types(UnresolvedDependency(..),
                                 Repo, AvailablePackageSource(..),
                                 AvailablePackage(AvailablePackage),
                                 AvailablePackageDb(AvailablePackageDb))
import Distribution.Client.Dependency as Dependency
         ( resolveAvailablePackages
         , dependencyConstraints, dependencyTargets
         , PackagesPreference(..), PackagesPreferenceDefault(..)
         , PackagePreference(..) )
import Distribution.Client.Fetch
        ( fetchPackage )
import Distribution.Client.HttpUtils
        ( downloadURI )
import qualified Distribution.Client.Tar as Tar (extractTarGzFile)
import Distribution.Client.IndexUtils as IndexUtils
    (getAvailablePackages, disambiguateDependencies)

import System.Directory
         ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
         , getTemporaryDirectory )
import System.IO
         ( openTempFile, hClose )
import Control.Monad
         ( unless, when )
import Data.Monoid
         ( mempty )
import System.FilePath
         ( (</>), addTrailingPathSeparator )
import qualified Data.Map as Map

unpack :: UnpackFlags -> [Repo] -> [Dependency] -> IO ()
unpack flags _ [] =
    notice verbosity "No packages requested. Nothing to do."
  where
    verbosity = fromFlag (unpackVerbosity flags)

unpack flags repos deps = do
  db@(AvailablePackageDb available _)
            <- getAvailablePackages verbosity repos
  deps' <- IndexUtils.disambiguateDependencies available
         . map toUnresolved $ deps

  pkgs <- resolvePackages db deps'

  unless (null prefix) $
         createDirectoryIfMissing True prefix

  flip mapM_ pkgs $ \pkg -> case pkg of

    AvailablePackage pkgid _ (LocalTarballPackage tarballPath) ->
      unpackPackage verbosity prefix pkgid tarballPath

    AvailablePackage pkgid _ (RemoteTarballPackage tarballURL) -> do
      tmp <- getTemporaryDirectory
      (tarballPath, hnd) <- openTempFile tmp (display pkgid)
      hClose hnd
      --TODO: perhaps we've already had to download this to a local cache
      --      so we even know what package version it is. So might be able
      --      to get it from the local cache rather than from remote.
      downloadURI verbosity tarballURL tarballPath
      unpackPackage verbosity prefix pkgid tarballPath

    AvailablePackage pkgid _ (RepoTarballPackage repo) -> do
      tarballPath <- fetchPackage verbosity repo pkgid
      unpackPackage verbosity prefix pkgid tarballPath

    AvailablePackage _ _ (LocalUnpackedPackage _) ->
      error "Distribution.Client.Unpack.unpack: the impossible happened."

    where
      verbosity = fromFlag (unpackVerbosity flags)
      prefix = fromFlagOrDefault "" (unpackDestDir flags)
      toUnresolved d = UnresolvedDependency d []

unpackPackage :: Verbosity -> FilePath -> PackageId -> FilePath -> IO ()
unpackPackage verbosity prefix pkgid pkgPath = do
    let pkgdirname = display pkgid
        pkgdir     = prefix </> pkgdirname
        pkgdir'    = addTrailingPathSeparator pkgdir
    existsDir  <- doesDirectoryExist pkgdir
    when existsDir $ die $
     "The directory \"" ++ pkgdir' ++ "\" already exists, not unpacking."
    existsFile  <- doesFileExist pkgdir
    when existsFile $ die $
     "A file \"" ++ pkgdir ++ "\" is in the way, not unpacking."
    notice verbosity $ "Unpacking to " ++ pkgdir'
    Tar.extractTarGzFile prefix pkgdirname pkgPath

resolvePackages :: AvailablePackageDb
                -> [UnresolvedDependency]
                -> IO [AvailablePackage]
resolvePackages
  (AvailablePackageDb available availablePrefs) deps =

    either (die . unlines . map show) return $
      resolveAvailablePackages
        installed   available
        preferences constraints
        targets

  where
    installed   = mempty
    targets     = dependencyTargets     deps
    constraints = dependencyConstraints deps
    preferences = PackagesPreference
                    PreferLatestForSelected
                    [ PackageVersionPreference name ver
                    | (name, ver) <- Map.toList availablePrefs ]

-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Install
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to package installation.
-----------------------------------------------------------------------------
module Hackage.Install
    ( install
    ) where

import Data.Monoid (Monoid(mconcat))
import Control.Exception (bracket_, try)
import Control.Monad (when)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing
                        ,removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>),(<.>))

import Hackage.Dependency (resolveDependencies, resolveDependenciesLocal, packagesToInstall)
import Hackage.Fetch (fetchPackage)
import qualified Hackage.IndexUtils as IndexUtils
import qualified Hackage.DepGraph as DepGraph
import Hackage.Tar (extractTarGzFile)
import Hackage.Types (UnresolvedDependency(..), PkgInfo(..), FlagAssignment,
                      Repo)
import Hackage.Utils (showDependencies)

import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Command (commandShowOptions)
import Distribution.Simple.SetupWrapper (setupWrapper)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.Package (showPackageId, PackageIdentifier(..))
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Utils as Utils (notice, info, debug, die)
import Distribution.Verbosity (Verbosity)


-- |Installs the packages needed to satisfy a list of dependencies.
install :: Verbosity
        -> PackageDB
        -> [Repo]
        -> Compiler
        -> ProgramConfiguration
        -> Cabal.ConfigFlags
        -> [UnresolvedDependency]
        -> IO ()
install verbosity packageDB repos comp conf configFlags deps
    | null deps = installLocalPackage verbosity packageDB repos comp conf configFlags
    | otherwise = installRepoPackages verbosity packageDB repos comp conf configFlags deps

-- | Install the unpacked package in the current directory, and all its dependencies.
installLocalPackage :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> Cabal.ConfigFlags
                    -> IO ()
installLocalPackage verbosity packageDB repos comp conf configFlags =
   do cabalFile <- defaultPackageDesc verbosity
      desc <- readPackageDescription verbosity cabalFile
      Just installed <- getInstalledPackages verbosity comp packageDB conf
      available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
      let resolvedDeps = resolveDependenciesLocal comp installed available desc
                           (Cabal.configConfigurationsFlags configFlags)
      case packagesToInstall resolvedDeps of
        Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
        Right pkgs   -> installPackages verbosity configFlags pkgs
      installUnpackedPkg verbosity configFlags Nothing

installRepoPackages :: Verbosity
                    -> PackageDB
                    -> [Repo]
                    -> Compiler
                    -> ProgramConfiguration
                    -> Cabal.ConfigFlags
                    -> [UnresolvedDependency]
                    -> IO ()
installRepoPackages verbosity packageDB repos comp conf configFlags deps =
    do Just installed <- getInstalledPackages verbosity comp packageDB conf
       available <- fmap mconcat (mapM (IndexUtils.readRepoIndex verbosity) repos)
       deps' <- IndexUtils.disambiguateDependencies available deps
       let resolvedDeps = resolveDependencies comp installed available deps'
       case packagesToInstall resolvedDeps of
         Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
         Right pkgs
           | DepGraph.empty pkgs -> notice verbosity
                     "All requested packages already installed. Nothing to do."
           | otherwise -> installPackages verbosity configFlags pkgs

installPackages :: Verbosity
                -> Cabal.ConfigFlags -- ^Options which will be passed to every package.
                -> DepGraph.DepGraph
                -> IO ()
installPackages verbosity configFlags pkgs = do 
  errorPackages <- installPackagesErrs [] pkgs
  case errorPackages of
    [] -> return ()
    errpkgs -> let errorMsg = concat $ "Error: some packages failed to install:"
                            : ["\n  " ++ showPackageId (pkgInfoId pkg)
                            | pkg <- errpkgs]
                in die errorMsg
  where
    installPackagesErrs failed remaining
      | DepGraph.empty remaining = return failed
      | otherwise = case DepGraph.ready remaining of
      DepGraph.ResolvedPackage pkg flags _depids -> do--TODO build against exactly these deps
        maybeInstalled <- try (installPkg verbosity configFlags pkg flags)
        case maybeInstalled of
          Left  _ ->
            let (remaining', failed') = DepGraph.removeFailed (pkgInfoId pkg) remaining
                failed'' = [ pkg' | DepGraph.ResolvedPackage pkg' _ _ <- failed' ]
             in installPackagesErrs (failed++failed'') remaining'
          Right _ ->
            let remaining' = DepGraph.removeCompleted (pkgInfoId pkg) remaining
             in installPackagesErrs failed remaining'

{-|
  Download, build and install a given package with some given flags.

  The process is divided up in a few steps:

    * The package is downloaded to {config-dir}\/packages\/{pkg-id} (if not already there).

    * The fetched tarball is then moved to a temporary directory (\/tmp on linux) and unpacked.

    * setupWrapper (equivalent to cabal-setup) is called with the options
      \'configure\' and the user specified options, \'--user\'
      if the 'configUser' flag is @True@ and install directory flags depending on 
      @configUserInstallDirs@ or @configGlobalInstallDirs@.

    * setupWrapper \'build\' is called with no options.

    * setupWrapper \'install\' is called with the \'--user\' flag if 'configUserInstall' is @True@.

    * The installation finishes by deleting the unpacked tarball.
-} 
installPkg :: Verbosity
           -> Cabal.ConfigFlags -- ^Options which will be parse to every package.
           -> PkgInfo
           -> FlagAssignment
           -> IO ()
installPkg verbosity configFlags pkg flags
    = do pkgPath <- fetchPackage verbosity pkg
         tmp <- getTemporaryDirectory
         let p = pkgInfoId pkg
             tmpDirPath = tmp </> ("TMP" ++ showPackageId p)
             path = tmpDirPath </> showPackageId p
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do info verbosity $ "Extracting " ++ pkgPath ++ " to " ++ tmpDirPath ++ "..."
                      extractTarGzFile (Just tmpDirPath) pkgPath
                      let descFilePath = tmpDirPath </> showPackageId p </> pkgName p <.> "cabal"
                      e <- doesFileExist descFilePath
                      when (not e) $ die $ "Package .cabal file not found: " ++ show descFilePath
                      let configFlags' = configFlags {
                            Cabal.configConfigurationsFlags =
                              Cabal.configConfigurationsFlags configFlags ++ flags }
                      installUnpackedPkg verbosity configFlags' (Just path))

installUnpackedPkg :: Verbosity
                   -> Cabal.ConfigFlags -- ^ Arguments for this package
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO ()
installUnpackedPkg verbosity configFlags mpath
    = do setup ("configure" : configureOptions)
         setup ["build"]
         setup ["install"]
  where
    configureCommand = Cabal.configureCommand defaultProgramConfiguration
    configureOptions = commandShowOptions configureCommand configFlags
    setup cmds
        = do debug verbosity $
               "setupWrapper in " ++ show mpath ++ " :\n " ++ show cmds
             setupWrapper cmds mpath

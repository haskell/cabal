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

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Monoid (Monoid(mempty))
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing
                        ,removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>),(<.>))

import Hackage.Dependency (resolveDependencies, resolveDependenciesLocal, packagesToInstall)
import Hackage.Fetch (fetchPackage)
import Hackage.Tar (extractTarGzFile)
import Hackage.Types (ConfigFlags(..), UnresolvedDependency(..)
                     , PkgInfo(..), FlagAssignment)
import Hackage.Utils

import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Program (ProgramConfiguration, defaultProgramConfiguration)
import Distribution.Simple.Command (commandShowOptions)
import Distribution.Simple.SetupWrapper (setupWrapper)
import Distribution.Simple.Setup (toFlag)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.Package (showPackageId, PackageIdentifier(..))
import Distribution.PackageDescription (readPackageDescription)
import Distribution.Simple.Utils as Utils (notice, info, debug, die)


-- |Installs the packages needed to satisfy a list of dependencies.
install :: ConfigFlags -> Compiler -> ProgramConfiguration -> Cabal.ConfigFlags -> [UnresolvedDependency] -> IO ()
install cfg comp conf configFlags deps
    | null deps = installLocalPackage cfg comp conf configFlags
    | otherwise = installRepoPackages cfg comp conf configFlags deps

-- | Install the unpacked package in the current directory, and all its dependencies.
installLocalPackage :: ConfigFlags -> Compiler -> ProgramConfiguration -> Cabal.ConfigFlags -> IO ()
installLocalPackage cfg comp conf configFlags =
   do cabalFile <- defaultPackageDesc (configVerbose cfg)
      desc <- readPackageDescription (configVerbose cfg) cabalFile
      resolvedDeps <- resolveDependenciesLocal cfg comp conf desc
                        (Cabal.configConfigurationsFlags configFlags)
      case packagesToInstall resolvedDeps of
        Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
        Right pkgs   -> installPackages cfg configFlags pkgs
      installUnpackedPkg cfg configFlags Nothing

installRepoPackages :: ConfigFlags -> Compiler -> ProgramConfiguration -> Cabal.ConfigFlags -> [UnresolvedDependency] -> IO ()
installRepoPackages cfg comp conf configFlags deps =
    do resolvedDeps <- resolveDependencies cfg comp conf deps
       case packagesToInstall resolvedDeps of
         Left missing -> die $ "Unresolved dependencies: " ++ showDependencies missing
         Right []     -> notice verbosity "All requested packages already installed. Nothing to do."
         Right pkgs   -> installPackages cfg configFlags pkgs
  where verbosity = configVerbose cfg

installPackages :: ConfigFlags
                -> Cabal.ConfigFlags -- ^Options which will be passed to every package.
                -> [(PkgInfo,FlagAssignment)] -- ^ (Package, list of configure options)
                -> IO ()
installPackages cfg configFlags = mapM_ (installPkg cfg configFlags)


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
installPkg :: ConfigFlags
           -> Cabal.ConfigFlags -- ^Options which will be parse to every package.
           -> (PkgInfo,FlagAssignment) -- ^(Package, list of configure options)
           -> IO ()
installPkg cfg configFlags (pkg,flags)
    = do pkgPath <- fetchPackage cfg pkg
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
                      installUnpackedPkg cfg configFlags' (Just path))
  where verbosity = configVerbose cfg

installUnpackedPkg :: ConfigFlags
                   -> Cabal.ConfigFlags -- ^ Arguments for this package
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO ()
installUnpackedPkg cfg configFlags mpath
    = do setup ("configure" : configureOptions)
         setup ["build"]
         setup ["install"]
  where
    configureOptions = mkPkgOps cfg configFlags
    setup cmds
        = do debug (configVerbose cfg) $
               "setupWrapper in " ++ show mpath ++ " :\n " ++ show cmds
             setupWrapper cmds mpath

-- Attach the correct prefix flag to configure commands,
-- correct --user flag to install commands and no options to other commands.
mkPkgOps :: ConfigFlags -> Cabal.ConfigFlags -> [String]
mkPkgOps cfg configFlags =
  commandShowOptions (Cabal.configureCommand defaultProgramConfiguration) configFlags {
    Cabal.configHcFlavor  = toFlag (configCompiler cfg),
    Cabal.configHcPath    = maybe (Cabal.configHcPath configFlags)
                                  toFlag (configCompilerPath cfg),
    Cabal.configHcPkg     = maybe (Cabal.configHcPkg configFlags)
                                  toFlag (configHcPkgPath cfg),
    Cabal.configInstallDirs = fmap (maybe mempty toFlag) installDirTemplates,
    Cabal.configVerbose   = toFlag (configVerbose cfg),
    Cabal.configPackageDB = if configUserInstall cfg
                              then toFlag UserPackageDB
                              else toFlag GlobalPackageDB
  }
 where installDirTemplates | configUserInstall cfg = configUserInstallDirs cfg
                           | otherwise             = configGlobalInstallDirs cfg

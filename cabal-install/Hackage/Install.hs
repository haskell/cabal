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
    ( install    -- :: ConfigFlags -> [UnresolvedDependency] -> IO ()
    , installPackages
    , installPkg -- :: ConfigFlags -> (PackageIdentifier,[String],String) -> IO ()
    ) where

import Control.Exception (bracket_)
import Control.Monad (when)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing
                        ,removeDirectoryRecursive, doesFileExist)
import System.FilePath ((</>),(<.>))

import Text.Printf (printf)


import Hackage.Config (message)
import Hackage.Dependency (resolveDependencies, resolveDependenciesLocal, packagesToInstall)
import Hackage.Fetch (fetchPackage)
import Hackage.Tar (extractTarGzFile)
import Hackage.Types (ConfigFlags(..), UnresolvedDependency(..)
                     , PkgInfo(..), pkgInfoId, ResolvedPackage)

import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Simple.InstallDirs (InstallDirs(..), absoluteInstallDirs)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.SetupWrapper (setupWrapper)
import Distribution.Simple.Setup (CopyDest(..))
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.Package (showPackageId, PackageIdentifier(..))
import Distribution.PackageDescription (packageDescription, readPackageDescription, package)
import Distribution.Verbosity




-- |Installs the packages needed to satisfy a list of dependencies.
install :: ConfigFlags -> Compiler -> ProgramConfiguration -> [String] -> [UnresolvedDependency] -> IO ()
install cfg comp conf globalArgs deps
    | null deps = installLocalPackage cfg comp conf globalArgs
    | otherwise = do resolvedDeps <- resolveDependencies cfg comp conf deps
                     installResolvedDeps cfg comp globalArgs resolvedDeps

-- | Install the unpacked package in the current directory, and all its dependencies.
installLocalPackage :: ConfigFlags -> Compiler -> ProgramConfiguration -> [String] -> IO ()
installLocalPackage cfg comp conf globalArgs =
   do cabalFile <- defaultPackageDesc (configVerbose cfg)
      desc <- readPackageDescription (configVerbose cfg) cabalFile
      resolvedDeps <- resolveDependenciesLocal cfg comp conf desc globalArgs
      installResolvedDeps cfg comp globalArgs resolvedDeps
      let pkgId = package (packageDescription desc)
      installUnpackedPkg cfg comp globalArgs pkgId [] Nothing

installResolvedDeps :: ConfigFlags -> Compiler -> [String] -> [ResolvedPackage] -> IO ()
installResolvedDeps cfg comp globalArgs resolvedDeps =
    case packagesToInstall resolvedDeps of
           Left missing -> fail $ "Unresolved dependencies: " ++ show missing
           Right pkgs   -> installPackages cfg comp globalArgs pkgs

-- Attach the correct prefix flag to configure commands,
-- correct --user flag to install commands and no options to other commands.
mkPkgOps :: ConfigFlags -> Compiler -> PackageIdentifier -> String -> [String] -> [String]
mkPkgOps cfg comp pkgId cmd ops = verbosity ++
  case cmd of
    "configure" -> user ++ installDirFlags installDirs ++ ops
    "install"   -> user
    _ -> []
 where verbosity = ["--verbose=" ++ showForCabal (configVerbose cfg)]
       user = if configUserInstall cfg then ["--user"] else []
       installDirs = absoluteInstallDirs pkgId (compilerId comp) NoCopyDest (configInstallDirs cfg)

installDirFlags :: InstallDirs FilePath -> [String]
installDirFlags dirs =
    [flag "prefix" prefix,
     flag "bindir" bindir,
     flag "libdir" libdir,
--     flag "dynlibdir" dynlibdir, -- not accepted as argument by cabal?
     flag "libexecdir" libexecdir,
--     flag "progdir" progdir, -- not accepted as argument by cabal?
--     flag "includedir" includedir, -- not accepted as argument by cabal?
     flag "datadir" datadir,
     flag "docdir" docdir,
     flag "htmldir" htmldir]
  where flag s f = "--" ++ s ++ "=" ++ f dirs

installPackages :: ConfigFlags
                -> Compiler
                -> [String] -- ^Options which will be parse to every package.
                -> [(PkgInfo,[String])] -- ^ (Package, list of configure options)
                -> IO ()
installPackages cfg comp globalArgs pkgs 
    | null pkgs = putStrLn "All requested packages already installed. Nothing to do."
    | otherwise = mapM_ (installPkg cfg comp globalArgs) pkgs

{-|
  Download, build and install a given package with some given flags.

  The process is divided up in a few steps:

    * The package is downloaded to {config-dir}\/packages\/{pkg-id} (if not already there).

    * The fetched tarball is then moved to a temporary directory (\/tmp on linux) and unpacked.

    * setupWrapper (equivalent to cabal-setup) is called with the options
      \'configure\' and the user specified options, \'--user\'
      if the 'configUser' flag is @True@ and install directory flags depending on @configInstallDirs@.

    * setupWrapper \'build\' is called with no options.

    * setupWrapper \'install\' is called with the \'--user\' flag if 'configUserInstall' is @True@.

    * The installation finishes by deleting the unpacked tarball.
-} 
installPkg :: ConfigFlags
           -> Compiler
           -> [String] -- ^Options which will be parse to every package.
           -> (PkgInfo,[String]) -- ^(Package, list of configure options)
           -> IO ()
installPkg cfg comp globalArgs (pkg,opts)
    = do pkgPath <- fetchPackage cfg pkg
         tmp <- getTemporaryDirectory
         let p = pkgInfoId pkg
             tmpDirPath = tmp </> printf "TMP%sTMP" (showPackageId p)
             path = tmpDirPath </> showPackageId p
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do message cfg verbose (printf "Extracting %s to %s..." pkgPath tmpDirPath)
                      extractTarGzFile (Just tmpDirPath) pkgPath
                      let descFilePath = tmpDirPath </> showPackageId p </> pkgName p <.> "cabal"
                      e <- doesFileExist descFilePath
                      when (not e) $ fail $ "Package .cabal file not found: " ++ show descFilePath
                      installUnpackedPkg cfg comp globalArgs p opts (Just path)
                      return ())

installUnpackedPkg :: ConfigFlags -> Compiler 
                   -> [String] -- ^ Arguments for all packages
                   -> PackageIdentifier
                   -> [String] -- ^ Arguments for this package
                   -> Maybe FilePath -- ^ Directory to change to before starting the installation.
                   -> IO ()
installUnpackedPkg cfg comp globalArgs pkgId opts mpath
    = do setup "configure"
         setup "build"
         setup "install"
  where
    setup cmd 
        = do let cmdOps = mkPkgOps cfg comp pkgId cmd (globalArgs++opts)
             message cfg verbose $ 
                     unwords ["setupWrapper", show (cmd:cmdOps), show mpath]
             setupWrapper (cmd:cmdOps) mpath
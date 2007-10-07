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


import Hackage.Config (findCompiler, message)
import Hackage.Dependency (getPackages, resolveDependencies)
import Hackage.Fetch (isFetched, packageFile, fetchPackage)
import Hackage.Tar (extractTarGzFile)
import Hackage.Types (ConfigFlags(..), UnresolvedDependency(..)
                                      , Repo(..))

import Distribution.Simple.Compiler (Compiler(..))
import Distribution.Simple.InstallDirs (InstallDirs(..), absoluteInstallDirs)
import Distribution.Simple.SetupWrapper (setupWrapper)
import Distribution.Simple.Setup (CopyDest(..))
import Distribution.Package (showPackageId, PackageIdentifier(..))
import Distribution.Verbosity




-- |Installs the packages needed to satisfy a list of dependencies.
install :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
install cfg globalArgs deps
    = do (comp,conf) <- findCompiler cfg
         resolvedDeps <- resolveDependencies cfg comp conf deps
         let apkgs = getPackages resolvedDeps
         if null apkgs
           then putStrLn "All requested packages already installed. Nothing to do."
           else installPackages cfg comp globalArgs apkgs

-- Fetch a package and output nice messages.
downloadPkg :: ConfigFlags -> PackageIdentifier -> Repo -> IO FilePath
downloadPkg cfg pkg repo
    = do fetched <- isFetched cfg pkg repo
         if fetched
            then do printf "'%s' is present.\n" (showPackageId pkg)
                    return (packageFile cfg pkg repo)
            else fetchPackage cfg pkg repo

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
                -> [(PackageIdentifier,[String],Repo)] -- ^(Package, list of configure options, package location)
                -> IO ()
installPackages cfg comp globalArgs pkgs =
       mapM_ (installPkg cfg comp globalArgs) pkgs

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
           -> (PackageIdentifier,[String],Repo) -- ^(Package, list of configure options, package location)
           -> IO ()
installPkg cfg comp globalArgs (pkg,ops,repo)
    = do pkgPath <- downloadPkg cfg pkg repo
         tmp <- getTemporaryDirectory
         let tmpDirPath = tmp </> printf "TMP%sTMP" (showPackageId pkg)
             setup cmd
                 = do let cmdOps = mkPkgOps cfg comp pkg cmd (globalArgs++ops)
                          path = tmpDirPath </> showPackageId pkg
                      message cfg deafening $ 
                                 unwords ["setupWrapper", show (cmd:cmdOps), show path]
                      setupWrapper (cmd:cmdOps) (Just path)
         bracket_ (createDirectoryIfMissing True tmpDirPath)
                  (removeDirectoryRecursive tmpDirPath)
                  (do message cfg deafening (printf "Extracting %s..." pkgPath)
                      extractTarGzFile (Just tmpDirPath) pkgPath
                      let descFilePath = tmpDirPath </> showPackageId pkg </> pkgName pkg <.> "cabal"
                      e <- doesFileExist descFilePath
                      when (not e) $ fail $ "Package .cabal file not found: " ++ show descFilePath
                      installUnpackedPkg cfg pkg setup
                      return ())

installUnpackedPkg :: ConfigFlags -> PackageIdentifier
                   -> (String -> IO ()) -> IO ()
installUnpackedPkg _cfg pkgId setup
    = do printf "Building '%s'\n" (showPackageId pkgId)
         printf "  Configuring...\n"
         setup "configure"
         printf "  Building...\n"
         setup "build"
         printf "  Installing...\n"
         setup "install"
         printf "  Done.\n"
         return ()

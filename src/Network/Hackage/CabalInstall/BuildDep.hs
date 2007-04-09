-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.BuildDep
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to a specialized instance of package installation.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.BuildDep where

import Network.Hackage.CabalInstall.Dependency (getPackages, getBuildDeps, depToUnresolvedDep, resolveDependenciesAux)
import Network.Hackage.CabalInstall.Install (install, installPkg)
import Network.Hackage.CabalInstall.Types (ConfigFlags (..), UnresolvedDependency)

import Distribution.PackageDescription (readPackageDescription, buildDepends)
import Distribution.Simple.Configure (getInstalledPackages)

{-|
  This function behaves exactly like 'Network.Hackage.CabalInstall.Install.install' except
  that it only builds the dependencies for packages.
-}
buildDep :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
buildDep cfg globalArgs deps
    = do ipkgs <- getInstalledPackages (configCompiler cfg) (configUserIns cfg) (configVerbose cfg)
         apkgs <- fmap getPackages (fmap (getBuildDeps ipkgs) (resolveDependenciesAux cfg ipkgs deps))
         mapM_ (installPkg cfg globalArgs) apkgs

-- | Takes the path to a .cabal file, and installs the build-dependencies listed there.
-- FIXME: what if the package uses hooks which modify the build-dependencies?
buildDepLocalPkg :: ConfigFlags -> FilePath -> IO ()
buildDepLocalPkg cfg pkgDescPath = 
    do pkgDesc <- readPackageDescription (configVerbose cfg) pkgDescPath
       let deps = map depToUnresolvedDep $ buildDepends pkgDesc
       install cfg [] deps

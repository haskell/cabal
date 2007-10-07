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

import Network.Hackage.CabalInstall.Config (findCompiler)
import Network.Hackage.CabalInstall.Dependency (getPackages, getBuildDeps
                                               , listInstalledPackages
                                               , depToUnresolvedDep, resolveDependenciesAux)
import Network.Hackage.CabalInstall.Install (install, installPackages)
import Network.Hackage.CabalInstall.Types (ConfigFlags (..), UnresolvedDependency)

import Distribution.PackageDescription (readPackageDescription, buildDepends,
                                        GenericPackageDescription(..))

{-|
  This function behaves exactly like 'Network.Hackage.CabalInstall.Install.install' except
  that it only builds the dependencies for packages.
-}
buildDep :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
buildDep cfg globalArgs deps
    = do (comp,conf) <- findCompiler cfg
         ipkgs <- listInstalledPackages cfg comp conf
         apkgs <- fmap getPackages (fmap (getBuildDeps ipkgs)
                                         (resolveDependenciesAux cfg comp conf ipkgs deps))
         installPackages cfg comp globalArgs apkgs

-- | Takes the path to a .cabal file, and installs the build-dependencies listed there.
-- FIXME: what if the package uses hooks which modify the build-dependencies?
buildDepLocalPkg :: ConfigFlags -> FilePath -> IO ()
buildDepLocalPkg cfg pkgDescPath = 
    do pkgDesc <- readPackageDescription (configVerbose cfg) pkgDescPath
       let deps = map depToUnresolvedDep $ buildDepends $ packageDescription pkgDesc
       install cfg [] deps

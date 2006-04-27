-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalGet.BuildDep
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to a specialized instance of package installation.
-----------------------------------------------------------------------------
module Network.Hackage.CabalGet.BuildDep where

import Network.Hackage.CabalGet.Dependency (getPackages, getBuildDeps, resolveDependenciesAux)
import Network.Hackage.CabalGet.Install (installPkg)
import Network.Hackage.CabalGet.Types (ConfigFlags (..), UnresolvedDependency)

import Distribution.Simple.Configure (getInstalledPackages)

{-|
  This function behaves exactly like 'Network.Hackage.CabalGet.Install.install' except
  that it only builds the dependencies for packages.
-}
buildDep :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
buildDep cfg globalArgs deps
    = do ipkgs <- getInstalledPackages (configCompiler cfg) (configUser cfg) (configVerbose cfg)
         apkgs <- fmap getPackages (fmap (getBuildDeps ipkgs) (resolveDependenciesAux cfg ipkgs deps))
         mapM_ (installPkg cfg globalArgs) apkgs

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalGet.Info
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to a dry-run package installation.
-----------------------------------------------------------------------------
module Network.Hackage.CabalGet.Info where

import Network.Hackage.CabalGet.Dependency (resolveDependencies, fulfillDependency)
import Network.Hackage.CabalGet.Fetch (isFetched, packageFile)
import Network.Hackage.CabalGet.Types (ConfigFlags(..), ResolvedPackage(..)
                                      ,UnresolvedDependency(..), OutputGen(..))

import Distribution.Package (PackageIdentifier)
import Distribution.Simple.Configure (getInstalledPackages)

import Data.Maybe (listToMaybe)

info :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
info cfg globalArgs deps
    = do ipkgs <- getInstalledPackages (configCompiler cfg) (configUser cfg) (configVerbose cfg)
         apkgs <- resolveDependencies cfg [] deps
         mapM_ (infoPkg cfg ipkgs globalArgs) apkgs

{-|
  'infoPkg' displays various information about a package.
  This information can be used to figure out what packages will be installed, from where they'll be downloaded
  and what options will be parsed to them.
-}
infoPkg :: ConfigFlags -> [PackageIdentifier] -> [String] -> ResolvedPackage -> IO ()
infoPkg cfg ipkgs _ (ResolvedPackage { fulfilling = dep
                                     , resolvedData = Nothing })
    = showOtherPackageInfo output installedPkg dep
    where installedPkg = listToMaybe (filter (fulfillDependency dep) ipkgs)
          output = configOutputGen cfg
infoPkg cfg ipkgs globalArgs (ResolvedPackage { fulfilling = dep
                                              , pkgOptions = ops
                                              , resolvedData = (Just (pkg,location,deps)) })
    = do fetched <- isFetched cfg pkg
         let pkgFile = if fetched then Just (packageFile cfg pkg) else Nothing
         showPackageInfo output pkgFile isInstalled (globalArgs ++ ops) dep (pkg,location,deps)
    where output = configOutputGen cfg
          isInstalled = pkg `elem` ipkgs

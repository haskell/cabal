-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Info
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to a dry-run package installation.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Info where

import Network.Hackage.CabalInstall.Config (pkgURL)
import Network.Hackage.CabalInstall.Dependency 
    (resolveDependencies, fulfillDependency, listInstalledPackages)
import Network.Hackage.CabalInstall.Fetch (isFetched, packageFile)
import Network.Hackage.CabalInstall.Types (ConfigFlags(..), ResolvedPackage(..)
                                      ,UnresolvedDependency(..))

import Distribution.Package (PackageIdentifier, showPackageId)
import Distribution.ParseUtils (showDependency)

import Data.Maybe (listToMaybe, fromMaybe)
import Text.Printf (printf)

info :: ConfigFlags -> [String] -> [UnresolvedDependency] -> IO ()
info cfg globalArgs deps
    = do ipkgs <- listInstalledPackages cfg
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
    = showOtherPkg installedPkg dep
    where installedPkg = listToMaybe (filter (fulfillDependency dep) ipkgs)
infoPkg cfg ipkgs globalArgs (ResolvedPackage { fulfilling = dep
                                              , pkgOptions = ops
                                              , resolvedData = (Just (pkg,repo,deps)) })
    = do fetched <- isFetched cfg pkg repo
         let pkgFile = if fetched then Just (packageFile cfg pkg repo) else Nothing
         showPkgInfo pkgFile isInstalled (globalArgs ++ ops) dep (pkg,repo,deps)
    where isInstalled = pkg `elem` ipkgs

showPkgInfo mbPath installed ops dep (pkg,repo,deps)
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   printf "    Using:     %s\n" (showPackageId pkg)
                   printf "    Installed: %s\n" (if installed then "Yes" else "No")
                   printf "    Depends:   %s\n" (showDeps deps)
                   printf "    Options:   %s\n" (unwords ops)
                   printf "    Location:  %s\n" (pkgURL pkg repo)
                   printf "    Local:     %s\n\n" (fromMaybe "*Not downloaded" mbPath)
    where
          showDeps = show . map showDep
          showDep dep = show (showDependency (fulfilling dep))

showOtherPkg mbPkg dep
              = do printf "  Package:     '%s'\n" (show $ showDependency dep)
                   case mbPkg of
                     Nothing  -> printf "    Not available!\n\n"
                     Just pkg -> do printf "    Using:     %s\n" (showPackageId pkg)
                                    printf "    Installed: Yes\n\n"

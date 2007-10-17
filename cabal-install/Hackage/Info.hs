-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Info
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to a dry-run package installation.
-----------------------------------------------------------------------------
module Hackage.Info where

import Hackage.Config
import Hackage.Dependency 
import Hackage.Fetch
import Hackage.Types 
import Hackage.Utils

import Distribution.Package (showPackageId)
import Distribution.ParseUtils (showDependency)
import Distribution.Simple.Compiler (Compiler)
import Distribution.Simple.Program (ProgramConfiguration)

import Data.List (intersperse, nubBy)
import Text.Printf (printf)

info :: ConfigFlags -> Compiler -> ProgramConfiguration -> [String] -> [UnresolvedDependency] -> IO ()
info cfg comp conf _globalArgs deps
    = do apkgs <- resolveDependencies cfg comp conf deps
         mapM_ (infoPkg cfg) $ flattenResolvedPackages apkgs
         case packagesToInstall apkgs of
           Left missing -> 
               do putStrLn "The requested packages cannot be installed, because of missing dependencies:"
                  putStrLn $ showDependencies missing
           Right pkgs  ->
               do putStrLn "These packages would be installed:"
                  putStrLn $ concat $ intersperse ", " [showPackageId (pkgInfoId pkg) | (pkg,_) <- pkgs]
                           

flattenResolvedPackages :: [ResolvedPackage] -> [ResolvedPackage]
flattenResolvedPackages = nubBy fulfillSame. concatMap flatten
    where flatten p@(Available _ _ _ deps) = p : flattenResolvedPackages deps
          flatten p = [p]
          fulfillSame a b = fulfills a == fulfills b

infoPkg :: ConfigFlags -> ResolvedPackage -> IO ()
infoPkg _ (Installed dep p)
    = do printf "  Requested:    %s\n" (show $ showDependency dep)
         printf "    Installed:  %s\n\n" (showPackageId p)
infoPkg cfg (Available dep pkg opts deps)
    = do fetched <- isFetched cfg pkg
         let pkgFile = if fetched then packageFile cfg pkg
                                  else  "*Not downloaded"
         printf "  Requested:    %s\n" (show $ showDependency dep)
         printf "    Using:      %s\n" (showPackageId (pkgInfoId pkg))
         printf "    Depends:    %s\n" (showDependencies $ map fulfills deps)
         printf "    Options:    %s\n" (unwords opts)
         printf "    Location:   %s\n" (pkgURL pkg)
         printf "    Local:      %s\n\n" pkgFile
infoPkg _ (Unavailable dep)
    = do printf "  Requested:    %s\n" (show $ showDependency dep)
         printf "    Not available!\n\n"

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
import Distribution.Simple.Utils as Utils (notice, info)

import Data.List (nubBy)

info :: ConfigFlags -> Compiler -> ProgramConfiguration -> [UnresolvedDependency] -> IO ()
info cfg comp conf deps
    = do apkgs <- resolveDependencies cfg comp conf deps
         details <- mapM (infoPkg cfg) (flattenResolvedPackages apkgs)
         Utils.info verbosity $ unlines (map ("  "++) (concat details))
         case packagesToInstall apkgs of
           Left missing -> notice verbosity $
                "The requested packages cannot be installed, because of missing dependencies:\n"
             ++ showDependencies missing

           Right pkgs -> notice verbosity $
               "These packages would be installed:\n"
             ++ unlines [showPackageId (pkgInfoId pkg) | (pkg,_) <- pkgs]
  where verbosity = configVerbose cfg

flattenResolvedPackages :: [ResolvedPackage] -> [ResolvedPackage]
flattenResolvedPackages = nubBy fulfillSame. concatMap flatten
    where flatten p@(Available _ _ _ deps) = p : flattenResolvedPackages deps
          flatten p = [p]
          fulfillSame a b = fulfills a == fulfills b

infoPkg :: ConfigFlags -> ResolvedPackage -> IO [String]
infoPkg _ (Installed dep p)
    = return ["Requested:    " ++ show (showDependency dep)
             ,"  Installed:  " ++ showPackageId p]
infoPkg cfg (Available dep pkg flags deps)
    = do fetched <- isFetched cfg pkg
         return ["Requested:    " ++ show (showDependency dep)
                ,"  Using:      " ++ showPackageId (pkgInfoId pkg)
                ,"  Depends:    " ++ showDependencies (map fulfills deps)
                ,"  Options:    " ++ unwords [ if set then flag else '-':flag
                                             | (flag, set) <- flags ]
                ,"  Location:   " ++ pkgURL pkg
                ,"  Local:      " ++ if fetched
                                        then packageFile cfg pkg
                                        else  "*Not downloaded"
                ]
infoPkg _ (Unavailable dep)
    = return ["Requested:    " ++ show (showDependency dep)
             ,"  Not available!"
             ]

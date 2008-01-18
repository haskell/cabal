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

import qualified Hackage.LocalIndex as LocalIndex
import qualified Hackage.RepoIndex  as RepoIndex
import Hackage.Dependency 
import Hackage.Fetch
import Hackage.Types 
import Hackage.Utils (showDependencies)

import Distribution.Package (showPackageId)
import Distribution.ParseUtils (showDependency)
import Distribution.Simple.Compiler (Compiler, PackageDB)
import Distribution.Simple.Program (ProgramConfiguration)
import Distribution.Simple.Utils as Utils (notice, info)
import Distribution.Verbosity (Verbosity)

import Data.List (nubBy)
import Data.Monoid (Monoid(mconcat))

info :: Verbosity
     -> PackageDB
     -> [Repo]
     -> Compiler
     -> ProgramConfiguration
     -> [UnresolvedDependency]
     -> IO ()
info verbosity packageDB repos comp conf deps
    = do installed <- LocalIndex.read verbosity comp conf packageDB 
         available <- fmap mconcat (mapM (RepoIndex.read verbosity) repos)
         let apkgs = resolveDependencies comp installed available deps
         details <- mapM infoPkg (flattenResolvedPackages apkgs)
         Utils.info verbosity $ unlines (map ("  "++) (concat details))
         case packagesToInstall apkgs of
           Left missing -> notice verbosity $
                "The requested packages cannot be installed, because of missing dependencies:\n"
             ++ showDependencies missing

           Right [] -> notice verbosity $
                "All requested packages already installed. Nothing to do."

           Right pkgs -> notice verbosity $
               "These packages would be installed:\n"
             ++ unlines [showPackageId (pkgInfoId pkg) | (pkg,_) <- pkgs]

flattenResolvedPackages :: [ResolvedPackage] -> [ResolvedPackage]
flattenResolvedPackages = nubBy fulfillSame. concatMap flatten
    where flatten p@(Available _ _ _ deps) = p : flattenResolvedPackages deps
          flatten p = [p]
          fulfillSame a b = fulfills a == fulfills b

infoPkg :: ResolvedPackage -> IO [String]
infoPkg (Installed dep p)
    = return ["Requested:    " ++ show (showDependency dep)
             ,"  Installed:  " ++ showPackageId p]
infoPkg (Available dep pkg flags deps)
    = do fetched <- isFetched pkg
         return ["Requested:    " ++ show (showDependency dep)
                ,"  Using:      " ++ showPackageId (pkgInfoId pkg)
                ,"  Depends:    " ++ showDependencies (map fulfills deps)
                ,"  Options:    " ++ unwords [ if set then flag else '-':flag
                                             | (flag, set) <- flags ]
                ,"  Location:   " ++ packageURL pkg
                ,"  Local:      " ++ if fetched
                                        then packageFile pkg
                                        else  "*Not downloaded"
                ]
infoPkg (Unavailable dep)
    = return ["Requested:    " ++ show (showDependency dep)
             ,"  Not available!"
             ]

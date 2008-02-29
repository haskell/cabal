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
module Hackage.Info (
    flattenResolvedDependencies,
    infoPkg
  ) where

import Hackage.Fetch
import Hackage.Types 
import Hackage.Utils (showDependencies)

import Distribution.Package (showPackageId, Package(..))
import Distribution.ParseUtils (showDependency)

import Data.List (nubBy)

flattenResolvedDependencies :: [ResolvedDependency] -> [ResolvedDependency]
flattenResolvedDependencies = nubBy fulfillSame. concatMap flatten
    where flatten p@(AvailableDependency _ _ _ deps) = p : flattenResolvedDependencies deps
          flatten p = [p]
          fulfillSame a b = fulfills a == fulfills b

infoPkg :: ResolvedDependency -> IO [String]
infoPkg (InstalledDependency dep p)
    = return ["Requested:    " ++ show (showDependency dep)
             ,"  Installed:  " ++ showPackageId p]
infoPkg (AvailableDependency dep pkg flags deps)
    = do fetched <- isFetched pkg
         return ["Requested:    " ++ show (showDependency dep)
                ,"  Using:      " ++ showPackageId (packageId pkg)
                ,"  Depends:    " ++ showDependencies (map fulfills deps)
                ,"  Options:    " ++ unwords [ if set then flag else '-':flag
                                             | (flag, set) <- flags ]
                ,"  Location:   " ++ packageURL pkg
                ,"  Local:      " ++ if fetched
                                        then packageFile pkg
                                        else  "*Not downloaded"
                ]
infoPkg (UnavailableDependency dep)
    = return ["Requested:    " ++ show (showDependency dep)
             ,"  Not available!"
             ]

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Info
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- High level interface to a dry-run package installation.
-----------------------------------------------------------------------------
module Distribution.Client.Info (
    flattenResolvedDependencies,
    infoPkg
  ) where

import Distribution.Client.Fetch
import Distribution.Client.Types
import Distribution.Client.Utils (showDependencies)

import Distribution.Package
         ( Package(..) )
import Distribution.Text
         ( display )

import Data.List (nubBy)

flattenResolvedDependencies :: [ResolvedDependency] -> [ResolvedDependency]
flattenResolvedDependencies = nubBy fulfillSame. concatMap flatten
    where flatten p@(AvailableDependency _ _ _ deps) = p : flattenResolvedDependencies deps
          flatten p = [p]
          fulfillSame a b = fulfills a == fulfills b

infoPkg :: ResolvedDependency -> IO [String]
infoPkg (InstalledDependency dep p)
    = return ["Requested:    " ++ display dep
             ,"  Installed:  " ++ display p]
infoPkg (AvailableDependency dep pkg flags deps)
    = do fetched <- isFetched pkg
         return ["Requested:    " ++ display dep
                ,"  Using:      " ++ display (packageId pkg)
                ,"  Depends:    " ++ showDependencies (map fulfills deps)
                ,"  Options:    " ++ unwords [ if set then flag else '-':flag
                                             | (flag, set) <- flags ]
                ,"  Location:   " ++ packageURL pkg
                ,"  Local:      " ++ if fetched
                                        then packageFile pkg
                                        else  "*Not downloaded"
                ]
infoPkg (UnavailableDependency dep)
    = return ["Requested:    " ++ display dep
             ,"  Not available!"
             ]

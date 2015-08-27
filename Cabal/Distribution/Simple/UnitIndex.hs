{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.UnitIndex
-- Copyright   :  (c) Edward Z. Yang 2015
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of units.  Unlike 'PackageIndex', this is indexed by 'PackageKey',
-- not 'InstalledPackageId'.  This doesn't really make a difference without
-- Backpack, but with Backpack there may be multiple 'InstalledPackageInfo's
-- per 'InstalledPackageId' (with different PackageKeys because they are different
-- units or instantiated differently.)
--
-- A 'PackageIndex' is still a useful thing to have, because dependency
-- resolution does operate on packages and not units.
--
module Distribution.Simple.UnitIndex (
  UnitIndex,

  -- * Creating an index
  fromList,

  -- * Updates
  merge,
  insert,

  -- * Queries

  -- ** Precise lookups
  lookupPackageKey,

  -- ** Bulk lookups
  allUnits,

  -- ** Special queries
  dependencyGraph,
  ) where

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Compat.Binary (Binary)

import GHC.Generics (Generic)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.Graph as Graph

newtype UnitIndex = UnitIndex (Map PackageKey InstalledPackageInfo)
    deriving (Generic, Show, Read)

instance Binary UnitIndex

fromList :: [InstalledPackageInfo] -> UnitIndex
fromList us = UnitIndex (Map.fromList [(packageKey u, u) | u <- us])

merge :: UnitIndex -> UnitIndex -> UnitIndex
merge (UnitIndex ix1) (UnitIndex ix2) = UnitIndex (Map.unionWith (\_ y -> y) ix1 ix2)

insert :: InstalledPackageInfo -> UnitIndex -> UnitIndex
insert u (UnitIndex ix) = UnitIndex (Map.insert (packageKey u) u ix)

lookupPackageKey :: UnitIndex -> PackageKey -> Maybe InstalledPackageInfo
lookupPackageKey (UnitIndex ix) key = Map.lookup key ix

allUnits :: UnitIndex -> [InstalledPackageInfo]
allUnits (UnitIndex ix) = Map.elems ix

dependencyGraph :: UnitIndex -> (Graph.Graph,
                                 Graph.Vertex -> InstalledPackageInfo,
                                 PackageKey -> Maybe Graph.Vertex)
dependencyGraph index = (graph, vertex_to_pkg, id_to_vertex)
  where
    graph = Array.listArray bounds
              [ [ v | Just v <- map id_to_vertex (unitDepends pkg) ]
              | pkg <- pkgs ]

    pkgs             = allUnits index
    vertices         = zip (map packageKey pkgs) [0..]
    vertex_map       = Map.fromList vertices
    id_to_vertex pid = Map.lookup pid vertex_map

    vertex_to_pkg vertex = pkgTable ! vertex

    pkgTable   = Array.listArray bounds pkgs
    topBound = length pkgs - 1
    bounds = (0, topBound)

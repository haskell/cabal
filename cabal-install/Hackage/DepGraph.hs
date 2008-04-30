-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.DepGraph
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Package dependency graph
--
-----------------------------------------------------------------------------
module Hackage.DepGraph (
  DepGraph, ResolvedPackage(..),
  fromList, toList,
  empty,
  ready,
  removeCompleted,
  removeFailed
  ) where

import Hackage.Types
import Distribution.Package
         ( PackageIdentifier, Package(..), PackageFixedDeps(..) )
import Distribution.PackageDescription
         ( FlagAssignment )
import Distribution.Text
         ( display )
import Distribution.Simple.Utils
         ( intercalate, equating )

import Data.List
         ( partition, intersect, nubBy )
import Control.Exception
         ( assert )

data ResolvedPackage = ResolvedPackage AvailablePackage FlagAssignment [PackageIdentifier]
  deriving Show

instance Package ResolvedPackage where
   packageId (ResolvedPackage p _ _) = packageId p

instance PackageFixedDeps ResolvedPackage where
   depends (ResolvedPackage _ _ d) = d

-- | A package dependency graph
--
-- * Invariant: the graph is acyclic
--
newtype DepGraph = DepGraph [ResolvedPackage]

-- | Build a dependency graph from a set of resolved packages.
--
-- * The dependencies must not by cyclic.
--
fromList :: [ResolvedPackage] -> DepGraph
fromList = DepGraph . nubBy (equating packageId)

toList :: DepGraph -> [ResolvedPackage]
toList (DepGraph g) = g

-- | Is the graph empty?
empty :: DepGraph -> Bool
empty (DepGraph g) = null g


-- | The next package, meaning a package which has no dependencies.
--
-- * The graph must be non-empty.
--
ready :: DepGraph -> ResolvedPackage
ready (DepGraph pkgs) =
  case [ pkg | pkg@(ResolvedPackage _ _ []) <- pkgs ] of
    []      -> error $ "DepGraph.head: internal error: no nodes with 0-outdegree\n"
                    ++ unlines (map show pkgs)
    (pkg:_) -> pkg


-- | Remove a package from the graph, getting back an updated graph.
--
-- * The package must exist in the graph.
-- * The package must have had no dependent packages.
--
removeCompleted :: PackageIdentifier -> DepGraph -> DepGraph
removeCompleted pkgid (DepGraph pkgs) =
  case partition isCompleted pkgs of
    ([_pkg], pkgs') -> DepGraph [ ResolvedPackage pkg fs (filter (/=pkgid) deps)
                                | ResolvedPackage pkg fs deps <- pkgs' ]
    _               -> error $ "DepGraph.removeCompleted: no such package "
                            ++ display pkgid
                            ++ "\nin DepGraph: "
                            ++ intercalate ", "
                                 (map (display . packageId) pkgs)

  where isCompleted = (==pkgid) . packageId

-- | Remove a package and all the packages that depend on it from the graph.
--
-- You get back an updated graph and a list of packages that were removed
-- (the given package will be first in that list).
--
-- * The package must exist in the graph.
--
removeFailed :: PackageIdentifier -> DepGraph -> (DepGraph, [ResolvedPackage])
removeFailed pkgid (DepGraph pkgs0) =
  case partition ((==pkgid) . packageId) pkgs0 of
    ([pkg], pkgs') -> case remove [pkg] [pkgid] pkgs' of
                        result -> assert (packageId p == pkgid) result
                          where (_,p:_) = result
    ((_:_),_)      -> error $ "DepGraph.removeFailed: internal error multiple instances of "
                           ++ display pkgid
    _              -> error $ "DepGraph.removeFailed: no such package "
                           ++ display pkgid

  where
    remove rmpkgs pkgids pkgs =
      case partition (not . null . intersect pkgids . depends) pkgs of
        ([], _)          -> (DepGraph pkgs, rmpkgs)
        (rmpkgs', pkgs') -> remove (rmpkgs ++ rmpkgs') pkgids' pkgs'
          where pkgids' = map packageId rmpkgs'

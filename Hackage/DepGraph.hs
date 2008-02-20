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
import Distribution.Package (PackageIdentifier)

import Data.List (partition, intersect)

data ResolvedPackage = ResolvedPackage PkgInfo FlagAssignment [PackageIdentifier]
  deriving Show

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
fromList = DepGraph

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
    _               -> error "DepGraph.removeCompleted: no such package"
  where isCompleted = (==pkgid) . pkgInfoId . pkginfo

-- | Remove a package and all the packages that depend on it from the graph.
--
-- You get back an updated graph and a list of packages that were removed
-- (the given package will be first in that list).
--
-- * The package must exist in the graph.
--
removeFailed :: PackageIdentifier -> DepGraph -> (DepGraph, [ResolvedPackage])
removeFailed pkgid (DepGraph pkgs0) =
  case partition ((==pkgid) . pkgInfoId . pkginfo) pkgs0 of
    ([pkg], pkgs') -> remove [pkg] [pkgid] pkgs'
    _              -> error "DepGraph.removeFailed: no such package"

  where
    remove rmpkgs pkgids pkgs =
      case partition (not . null . intersect pkgids . pkgdeps) pkgs of
        ([], _)          -> (DepGraph pkgs, rmpkgs)
        (rmpkgs', pkgs') -> remove (rmpkgs ++ rmpkgs') pkgids' pkgs'
          where pkgids' = map (pkgInfoId.pkginfo) rmpkgs'

pkginfo :: ResolvedPackage -> PkgInfo
pkginfo (ResolvedPackage p _ _) = p

pkgdeps :: ResolvedPackage -> [PackageIdentifier]
pkgdeps (ResolvedPackage _ _ d) = d

{-# LANGUAGE CPP #-}
module Distribution.Client.PlanIndex (
    brokenPackages
  , dependencyClosure
  , dependencyCycles
  , dependencyGraph
  , dependencyInconsistencies
  , reverseDependencyClosure
  , reverseTopologicalOrder
  , topologicalOrder
  ) where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Tree  as Tree
import qualified Data.Graph as Graph
import qualified Data.Array as Array
import Data.Array ((!))
import Data.List (groupBy, sortBy, nub)
import Data.Maybe (isNothing, fromMaybe, catMaybes)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
#endif

import Distribution.Package
         ( PackageName(..), PackageIdentifier(..)
         , Package(..), packageName, packageVersion
         )
import Distribution.Version
         ( Version )
import Distribution.Simple.Utils (equating, comparing)

import Distribution.Client.PackageIndex
         ( PackageFixedDeps(..) )
import Distribution.Client.PackageIndex
         ( PackageIndex, lookupPackageId, allPackages, insert )

-- | All packages that have dependencies that are not in the index.
--
-- Returns such packages along with the dependencies that they're missing.
--
brokenPackages :: PackageFixedDeps pkg
               => PackageIndex pkg
               -> [(pkg, [PackageIdentifier])]
brokenPackages index =
  [ (pkg, missing)
  | pkg  <- allPackages index
  , let missing = [ pkg' | pkg' <- depends pkg
                         , isNothing (lookupPackageId index pkg') ]
  , not (null missing) ]

-- | Given a package index where we assume we want to use all the packages
-- (use 'dependencyClosure' if you need to get such a index subset) find out
-- if the dependencies within it use consistent versions of each package.
-- Return all cases where multiple packages depend on different versions of
-- some other package.
--
-- Each element in the result is a package name along with the packages that
-- depend on it and the versions they require. These are guaranteed to be
-- distinct.
--
dependencyInconsistencies :: PackageFixedDeps pkg
                          => PackageIndex pkg
                          -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies index =
  [ (name, inconsistencies)
  | (name, uses) <- Map.toList inverseIndex
  , let inconsistencies = duplicatesBy uses
        versions = map snd inconsistencies
  , reallyIsInconsistent name (nub versions) ]

  where inverseIndex = Map.fromListWith (++)
          [ (packageName dep, [(packageId pkg, packageVersion dep)])
          | pkg <- allPackages index
          , dep <- depends pkg ]

        duplicatesBy = (\groups -> if length groups == 1
                                     then []
                                     else concat groups)
                     . groupBy (equating snd)
                     . sortBy (comparing snd)

        reallyIsInconsistent :: PackageName -> [Version] -> Bool
        reallyIsInconsistent _    []       = False
        reallyIsInconsistent name [v1, v2] =
          case (mpkg1, mpkg2) of
            (Just pkg1, Just pkg2) -> pkgid1 `notElem` depends pkg2
                                   && pkgid2 `notElem` depends pkg1
            _ -> True
          where
            pkgid1 = PackageIdentifier name v1
            pkgid2 = PackageIdentifier name v2
            mpkg1 = lookupPackageId index pkgid1
            mpkg2 = lookupPackageId index pkgid2

        reallyIsInconsistent _ _ = True

-- | Find if there are any cycles in the dependency graph. If there are no
-- cycles the result is @[]@.
--
-- This actually computes the strongly connected components. So it gives us a
-- list of groups of packages where within each group they all depend on each
-- other, directly or indirectly.
--
dependencyCycles :: PackageFixedDeps pkg
                 => PackageIndex pkg
                 -> [[pkg]]
dependencyCycles index =
  [ vs | Graph.CyclicSCC vs <- Graph.stronglyConnComp adjacencyList ]
  where
    adjacencyList = [ (pkg, packageId pkg, depends pkg)
                    | pkg <- allPackages index ]

-- | Tries to take the transitive closure of the package dependencies.
--
-- If the transitive closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if the result is @Right []@ it is because at least one of
-- the original given 'PackageIdentifier's do not occur in the index.
--
dependencyClosure :: PackageFixedDeps pkg
                  => PackageIndex pkg
                  -> [PackageIdentifier]
                  -> Either (PackageIndex pkg)
                            [(pkg, [PackageIdentifier])]
dependencyClosure index pkgids0 = case closure mempty [] pkgids0 of
  (completed, []) -> Left completed
  (completed, _)  -> Right (brokenPackages completed)
  where
    closure completed failed []             = (completed, failed)
    closure completed failed (pkgid:pkgids) = case lookupPackageId index pkgid of
      Nothing   -> closure completed (pkgid:failed) pkgids
      Just pkg  -> case lookupPackageId completed (packageId pkg) of
        Just _  -> closure completed  failed pkgids
        Nothing -> closure completed' failed pkgids'
          where completed' = insert pkg completed
                pkgids'    = depends pkg ++ pkgids

topologicalOrder :: PackageFixedDeps pkg => PackageIndex pkg -> [pkg]
topologicalOrder index = map toPkgId
                       . Graph.topSort
                       $ graph
  where (graph, toPkgId, _) = dependencyGraph index

reverseTopologicalOrder :: PackageFixedDeps pkg => PackageIndex pkg -> [pkg]
reverseTopologicalOrder index = map toPkgId
                              . Graph.topSort
                              . Graph.transposeG
                              $ graph
  where (graph, toPkgId, _) = dependencyGraph index

-- | Takes the transitive closure of the packages reverse dependencies.
--
-- * The given 'PackageIdentifier's must be in the index.
--
reverseDependencyClosure :: PackageFixedDeps pkg
                         => PackageIndex pkg
                         -> [PackageIdentifier]
                         -> [pkg]
reverseDependencyClosure index =
    map vertexToPkg
  . concatMap Tree.flatten
  . Graph.dfs reverseDepGraph
  . map (fromMaybe noSuchPkgId . pkgIdToVertex)

  where
    (depGraph, vertexToPkg, pkgIdToVertex) = dependencyGraph index
    reverseDepGraph = Graph.transposeG depGraph
    noSuchPkgId = error "reverseDependencyClosure: package is not in the graph"




-- | Builds a graph of the package dependencies.
--
-- Dependencies on other packages that are not in the index are discarded.
-- You can check if there are any such dependencies with 'brokenPackages'.
--
dependencyGraph :: PackageFixedDeps pkg
                => PackageIndex pkg
                -> (Graph.Graph,
                    Graph.Vertex -> pkg,
                    PackageIdentifier -> Maybe Graph.Vertex)
dependencyGraph index = (graph, vertexToPkg, pkgIdToVertex)
  where
    graph = Array.listArray bounds $
            map (catMaybes . map pkgIdToVertex . depends) pkgs
    vertexToPkg vertex = pkgTable ! vertex
    pkgIdToVertex = binarySearch 0 topBound

    pkgTable   = Array.listArray bounds pkgs
    pkgIdTable = Array.listArray bounds (map packageId pkgs)
    pkgs = sortBy (comparing packageId) (allPackages index)
    topBound = length pkgs - 1
    bounds = (0, topBound)

    binarySearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (pkgIdTable ! mid) of
          LT -> binarySearch a (mid-1) key
          EQ -> Just mid
          GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2

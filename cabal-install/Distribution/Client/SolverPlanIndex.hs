{-# LANGUAGE ScopedTypeVariables #-}

-- | These graph traversal functions mirror the ones in Cabal, but work with
-- the more complete (and fine-grained) set of dependencies provided by
-- PackageFixedDeps rather than only the library dependencies provided by
-- PackageInstalled.
module Distribution.Client.SolverPlanIndex (
    -- * Graph traversal functions
    brokenPackages
  , dependencyCycles
  , dependencyGraph
  , dependencyInconsistencies
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

-- import Prelude hiding (lookup)
import qualified Data.Map as Map
import qualified Data.Graph as Graph
import Data.Array ((!))
-- import Data.Maybe (isNothing)
import Data.Either (rights)

import Distribution.Package
         ( PackageName, PackageIdentifier(..), UnitId
         , Package(..), packageName, packageVersion
         )
import Distribution.Version
         ( Version )

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.PackageFixedDeps
import           Distribution.Solver.Types.Settings

import Distribution.Simple.PackageIndex
         ( PackageIndex, allPackages, insert, lookupUnitId )
import Distribution.Package
         ( HasUnitId(..), PackageId )

-- | All packages that have dependencies that are not in the index.
--
-- Returns such packages along with the dependencies that they're missing.
--
brokenPackages :: (PackageFixedDeps pkg)
               => PackageIndex pkg
               -> [(pkg, [UnitId])]
brokenPackages index =
  [ (pkg, missing)
  | pkg  <- allPackages index
  , let missing =
          [ pkg' | pkg' <- CD.flatDeps (depends pkg)
                 , isNothing (lookupUnitId index pkg') ]
  , not (null missing) ]

-- | Compute all roots of the install plan, and verify that the transitive
-- plans from those roots are all consistent.
--
-- NOTE: This does not check for dependency cycles. Moreover, dependency cycles
-- may be absent from the subplans even if the larger plan contains a dependency
-- cycle. Such cycles may or may not be an issue; either way, we don't check
-- for them here.
dependencyInconsistencies :: forall pkg. (PackageFixedDeps pkg, HasUnitId pkg)
                          => IndependentGoals
                          -> PackageIndex pkg
                          -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies indepGoals index  =
    concatMap dependencyInconsistencies' subplans
  where
    subplans :: [PackageIndex pkg]
    subplans = rights $
                 map (dependencyClosure index)
                     (rootSets indepGoals index)

-- | Compute the root sets of a plan
--
-- A root set is a set of packages whose dependency closure must be consistent.
-- This is the set of all top-level library roots (taken together normally, or
-- as singletons sets if we are considering them as independent goals), along
-- with all setup dependencies of all packages.
rootSets :: (PackageFixedDeps pkg, HasUnitId pkg)
         => IndependentGoals -> PackageIndex pkg -> [[UnitId]]
rootSets (IndependentGoals indepGoals) index =
       if indepGoals then map (:[]) libRoots else [libRoots]
    ++ setupRoots index
  where
    libRoots = libraryRoots index

-- | Compute the library roots of a plan
--
-- The library roots are the set of packages with no reverse dependencies
-- (no reverse library dependencies but also no reverse setup dependencies).
libraryRoots :: (PackageFixedDeps pkg, HasUnitId pkg)
             => PackageIndex pkg -> [UnitId]
libraryRoots index =
    map toPkgId roots
  where
    (graph, toPkgId, _) = dependencyGraph index
    indegree = Graph.indegree graph
    roots    = filter isRoot (Graph.vertices graph)
    isRoot v = indegree ! v == 0

-- | The setup dependencies of each package in the plan
setupRoots :: PackageFixedDeps pkg => PackageIndex pkg -> [[UnitId]]
setupRoots = filter (not . null)
           . map (CD.setupDeps . depends)
           . allPackages

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
dependencyInconsistencies' :: forall pkg.
                              (PackageFixedDeps pkg, HasUnitId pkg)
                           => PackageIndex pkg
                           -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies' index =
    [ (name, [ (pid,packageVersion dep) | (dep,pids) <- uses, pid <- pids])
    | (name, ipid_map) <- Map.toList inverseIndex
    , let uses = Map.elems ipid_map
    , reallyIsInconsistent (map fst uses)
    ]
  where
    -- For each package name (of a dependency, somewhere)
    --   and each installed ID of that that package
    --     the associated package instance
    --     and a list of reverse dependencies (as source IDs)
    inverseIndex :: Map PackageName (Map UnitId (pkg, [PackageId]))
    inverseIndex = Map.fromListWith (Map.unionWith (\(a,b) (_,b') -> (a,b++b')))
      [ (packageName dep, Map.fromList [(ipid,(dep,[packageId pkg]))])
      | -- For each package @pkg@
        pkg <- allPackages index
        -- Find out which @ipid@ @pkg@ depends on
      , ipid <- CD.nonSetupDeps (depends pkg)
        -- And look up those @ipid@ (i.e., @ipid@ is the ID of @dep@)
      , Just dep <- [lookupUnitId index ipid]
      ]

    -- If, in a single install plan, we depend on more than one version of a
    -- package, then this is ONLY okay in the (rather special) case that we
    -- depend on precisely two versions of that package, and one of them
    -- depends on the other. This is necessary for example for the base where
    -- we have base-3 depending on base-4.
    reallyIsInconsistent :: [pkg] -> Bool
    reallyIsInconsistent []       = False
    reallyIsInconsistent [_p]     = False
    reallyIsInconsistent [p1, p2] =
      let pid1 = installedUnitId p1
          pid2 = installedUnitId p2
      in pid1 `notElem` CD.nonSetupDeps (depends p2)
      && pid2 `notElem` CD.nonSetupDeps (depends p1)
    reallyIsInconsistent _ = True



-- | Find if there are any cycles in the dependency graph. If there are no
-- cycles the result is @[]@.
--
-- This actually computes the strongly connected components. So it gives us a
-- list of groups of packages where within each group they all depend on each
-- other, directly or indirectly.
--
dependencyCycles :: (PackageFixedDeps pkg, HasUnitId pkg)
                 => PackageIndex pkg
                 -> [[pkg]]
dependencyCycles index =
  [ vs | Graph.CyclicSCC vs <- Graph.stronglyConnComp adjacencyList ]
  where
    adjacencyList = [ (pkg, installedUnitId pkg,
                            CD.flatDeps (depends pkg))
                    | pkg <- allPackages index ]


-- | Tries to take the transitive closure of the package dependencies.
--
-- If the transitive closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if the result is @Right []@ it is because at least one of
-- the original given 'PackageIdentifier's do not occur in the index.
dependencyClosure :: (PackageFixedDeps pkg, HasUnitId pkg)
                  => PackageIndex pkg
                  -> [UnitId]
                  -> Either [(pkg, [UnitId])]
                            (PackageIndex pkg)
dependencyClosure index pkgids0 = case closure mempty [] pkgids0 of
  (completed, []) -> Right completed
  (completed, _)  -> Left (brokenPackages completed)
 where
    closure completed failed []             = (completed, failed)
    closure completed failed (pkgid:pkgids) =
      case lookupUnitId index pkgid of
        Nothing   -> closure completed (pkgid:failed) pkgids
        Just pkg  ->
          case lookupUnitId completed
               (installedUnitId pkg) of
            Just _  -> closure completed  failed pkgids
            Nothing -> closure completed' failed pkgids'
              where completed' = insert pkg completed
                    pkgids'    = CD.nonSetupDeps (depends pkg) ++ pkgids


-- | Builds a graph of the package dependencies.
--
-- Dependencies on other packages that are not in the index are discarded.
-- You can check if there are any such dependencies with 'brokenPackages'.
--
dependencyGraph :: (PackageFixedDeps pkg, HasUnitId pkg)
                => PackageIndex pkg
                -> (Graph.Graph,
                    Graph.Vertex -> UnitId,
                    UnitId -> Maybe Graph.Vertex)
dependencyGraph index = (graph, vertexToPkg, idToVertex)
  where
    (graph, vertexToPkg', idToVertex) = Graph.graphFromEdges edges
    vertexToPkg v = case vertexToPkg' v of
                      ((), pkgid, _targets) -> pkgid

    pkgs  = allPackages index
    edges = map edgesFrom pkgs

    resolve   pid = pid
    edgesFrom pkg = ( ()
                    , resolve (installedUnitId pkg)
                    , CD.flatDeps (depends pkg)
                    )

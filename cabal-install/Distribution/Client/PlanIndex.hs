-- | These graph traversal functions mirror the ones in Cabal, but work with
-- the more complete (and fine-grained) set of dependencies provided by
-- PackageFixedDeps rather than only the library dependencies provided by
-- PackageInstalled.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Distribution.Client.PlanIndex (
    -- * FakeMap and related operations
    FakeMap
  , fakeDepends
  , fakeLookupComponentId
    -- * Graph traversal functions
  , brokenPackages
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
import Data.Array ((!))
import Data.Map (Map)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Data.Either (rights)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
#endif

import Distribution.Package
         ( PackageName(..), PackageIdentifier(..), ComponentId(..)
         , Package(..), packageName, packageVersion
         )
import Distribution.Version
         ( Version )

import Distribution.Client.ComponentDeps (ComponentDeps)
import qualified Distribution.Client.ComponentDeps as CD
import Distribution.Client.Types
         ( PackageFixedDeps(..) )
import Distribution.Simple.PackageIndex
         ( PackageIndex, allPackages, insert, lookupComponentId )
import Distribution.Package
         ( HasComponentId(..), PackageId )

-- Note [FakeMap]
-----------------
-- We'd like to use the PackageIndex defined in this module for
-- cabal-install's InstallPlan.  However, at the moment, this
-- data structure is indexed by ComponentId, which we don't
-- know until after we've compiled a package (whereas InstallPlan
-- needs to store not-compiled packages in the index.) Eventually,
-- an ComponentId will be calculatable prior to actually
-- building the package (making it something of a misnomer), but
-- at the moment, the "fake installed package ID map" is a workaround
-- to solve this problem while reusing PackageIndex.  The basic idea
-- is that, since we don't know what an ComponentId is
-- beforehand, we just fake up one based on the package ID (it only
-- needs to be unique for the particular install plan), and fill
-- it out with the actual generated ComponentId after the
-- package is successfully compiled.
--
-- However, there is a problem: in the index there may be
-- references using the old package ID, which are now dangling if
-- we update the ComponentId.  We could map over the entire
-- index to update these pointers as well (a costly operation), but
-- instead, we've chosen to parametrize a variety of important functions
-- by a FakeMap, which records what a fake installed package ID was
-- actually resolved to post-compilation.  If we do a lookup, we first
-- check and see if it's a fake ID in the FakeMap.
--
-- It's a bit grungy, but we expect this to only be temporary anyway.
-- (Another possible workaround would have been to *not* update
-- the installed package ID, but I decided this would be hard to
-- understand.)

-- | Map from fake package keys to real ones.  See Note [FakeMap]
type FakeMap = Map ComponentId ComponentId

-- | Variant of `depends` which accepts a `FakeMap`
--
-- Analogous to `fakeInstalledDepends`. See Note [FakeMap].
fakeDepends :: PackageFixedDeps pkg => FakeMap -> pkg -> ComponentDeps [ComponentId]
fakeDepends fakeMap = fmap (map resolveFakeId) . depends
  where
    resolveFakeId :: ComponentId -> ComponentId
    resolveFakeId ipid = Map.findWithDefault ipid ipid fakeMap

--- | Variant of 'lookupComponentId' which accepts a 'FakeMap'.  See Note
--- [FakeMap].
fakeLookupComponentId :: FakeMap -> PackageIndex a -> ComponentId
                             -> Maybe a
fakeLookupComponentId fakeMap index pkg =
  lookupComponentId index (Map.findWithDefault pkg pkg fakeMap)

-- | All packages that have dependencies that are not in the index.
--
-- Returns such packages along with the dependencies that they're missing.
--
brokenPackages :: (PackageFixedDeps pkg)
               => FakeMap
               -> PackageIndex pkg
               -> [(pkg, [ComponentId])]
brokenPackages fakeMap index =
  [ (pkg, missing)
  | pkg  <- allPackages index
  , let missing =
          [ pkg' | pkg' <- CD.nonSetupDeps (depends pkg)
                 , isNothing (fakeLookupComponentId fakeMap index pkg') ]
  , not (null missing) ]

-- | Compute all roots of the install plan, and verify that the transitive
-- plans from those roots are all consistent.
--
-- NOTE: This does not check for dependency cycles. Moreover, dependency cycles
-- may be absent from the subplans even if the larger plan contains a dependency
-- cycle. Such cycles may or may not be an issue; either way, we don't check
-- for them here.
dependencyInconsistencies :: forall pkg. (PackageFixedDeps pkg, HasComponentId pkg)
                          => FakeMap
                          -> Bool
                          -> PackageIndex pkg
                          -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies fakeMap indepGoals index  =
    concatMap (dependencyInconsistencies' fakeMap) subplans
  where
    subplans :: [PackageIndex pkg]
    subplans = rights $
                 map (dependencyClosure fakeMap index)
                     (rootSets fakeMap indepGoals index)

-- | Compute the root sets of a plan
--
-- A root set is a set of packages whose dependency closure must be consistent.
-- This is the set of all top-level library roots (taken together normally, or
-- as singletons sets if we are considering them as independent goals), along
-- with all setup dependencies of all packages.
rootSets :: (PackageFixedDeps pkg, HasComponentId pkg)
         => FakeMap -> Bool -> PackageIndex pkg -> [[ComponentId]]
rootSets fakeMap indepGoals index =
       if indepGoals then map (:[]) libRoots else [libRoots]
    ++ setupRoots index
  where
    libRoots = libraryRoots fakeMap index

-- | Compute the library roots of a plan
--
-- The library roots are the set of packages with no reverse dependencies
-- (no reverse library dependencies but also no reverse setup dependencies).
libraryRoots :: (PackageFixedDeps pkg, HasComponentId pkg)
             => FakeMap -> PackageIndex pkg -> [ComponentId]
libraryRoots fakeMap index =
    map (installedComponentId . toPkgId) roots
  where
    (graph, toPkgId, _) = dependencyGraph fakeMap index
    indegree = Graph.indegree graph
    roots    = filter isRoot (Graph.vertices graph)
    isRoot v = indegree ! v == 0

-- | The setup dependencies of each package in the plan
setupRoots :: PackageFixedDeps pkg => PackageIndex pkg -> [[ComponentId]]
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
                              (PackageFixedDeps pkg, HasComponentId pkg)
                           => FakeMap
                           -> PackageIndex pkg
                           -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies' fakeMap index =
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
    inverseIndex :: Map PackageName (Map ComponentId (pkg, [PackageId]))
    inverseIndex = Map.fromListWith (Map.unionWith (\(a,b) (_,b') -> (a,b++b')))
      [ (packageName dep, Map.fromList [(ipid,(dep,[packageId pkg]))])
      | -- For each package @pkg@
        pkg <- allPackages index
        -- Find out which @ipid@ @pkg@ depends on
      , ipid <- CD.nonSetupDeps (fakeDepends fakeMap pkg)
        -- And look up those @ipid@ (i.e., @ipid@ is the ID of @dep@)
      , Just dep <- [fakeLookupComponentId fakeMap index ipid]
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
      let pid1 = installedComponentId p1
          pid2 = installedComponentId p2
      in Map.findWithDefault pid1 pid1 fakeMap `notElem` CD.nonSetupDeps (fakeDepends fakeMap p2)
      && Map.findWithDefault pid2 pid2 fakeMap `notElem` CD.nonSetupDeps (fakeDepends fakeMap p1)
    reallyIsInconsistent _ = True




-- | Find if there are any cycles in the dependency graph. If there are no
-- cycles the result is @[]@.
--
-- This actually computes the strongly connected components. So it gives us a
-- list of groups of packages where within each group they all depend on each
-- other, directly or indirectly.
--
dependencyCycles :: (PackageFixedDeps pkg, HasComponentId pkg)
                 => FakeMap
                 -> PackageIndex pkg
                 -> [[pkg]]
dependencyCycles fakeMap index =
  [ vs | Graph.CyclicSCC vs <- Graph.stronglyConnComp adjacencyList ]
  where
    adjacencyList = [ (pkg, installedComponentId pkg, CD.nonSetupDeps (fakeDepends fakeMap pkg))
                    | pkg <- allPackages index ]


-- | Tries to take the transitive closure of the package dependencies.
--
-- If the transitive closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if the result is @Right []@ it is because at least one of
-- the original given 'PackageIdentifier's do not occur in the index.
dependencyClosure :: (PackageFixedDeps pkg, HasComponentId pkg)
                  => FakeMap
                  -> PackageIndex pkg
                  -> [ComponentId]
                  -> Either [(pkg, [ComponentId])]
                            (PackageIndex pkg)
dependencyClosure fakeMap index pkgids0 = case closure mempty [] pkgids0 of
  (completed, []) -> Right completed
  (completed, _)  -> Left (brokenPackages fakeMap completed)
 where
    closure completed failed []             = (completed, failed)
    closure completed failed (pkgid:pkgids) =
      case fakeLookupComponentId fakeMap index pkgid of
        Nothing   -> closure completed (pkgid:failed) pkgids
        Just pkg  ->
          case fakeLookupComponentId fakeMap completed
               (installedComponentId pkg) of
            Just _  -> closure completed  failed pkgids
            Nothing -> closure completed' failed pkgids'
              where completed' = insert pkg completed
                    pkgids'    = CD.nonSetupDeps (depends pkg) ++ pkgids


topologicalOrder :: (PackageFixedDeps pkg, HasComponentId pkg)
                 => FakeMap -> PackageIndex pkg -> [pkg]
topologicalOrder fakeMap index = map toPkgId
                               . Graph.topSort
                               $ graph
  where (graph, toPkgId, _) = dependencyGraph fakeMap index


reverseTopologicalOrder :: (PackageFixedDeps pkg, HasComponentId pkg)
                        => FakeMap -> PackageIndex pkg -> [pkg]
reverseTopologicalOrder fakeMap index = map toPkgId
                                      . Graph.topSort
                                      . Graph.transposeG
                                      $ graph
  where (graph, toPkgId, _) = dependencyGraph fakeMap index


-- | Takes the transitive closure of the packages reverse dependencies.
--
-- * The given 'PackageIdentifier's must be in the index.
--
reverseDependencyClosure :: (PackageFixedDeps pkg, HasComponentId pkg)
                         => FakeMap
                         -> PackageIndex pkg
                         -> [ComponentId]
                         -> [pkg]
reverseDependencyClosure fakeMap index =
    map vertexToPkg
  . concatMap Tree.flatten
  . Graph.dfs reverseDepGraph
  . map (fromMaybe noSuchPkgId . pkgIdToVertex)

  where
    (depGraph, vertexToPkg, pkgIdToVertex) = dependencyGraph fakeMap index
    reverseDepGraph = Graph.transposeG depGraph
    noSuchPkgId = error "reverseDependencyClosure: package is not in the graph"



-- | Builds a graph of the package dependencies.
--
-- Dependencies on other packages that are not in the index are discarded.
-- You can check if there are any such dependencies with 'brokenPackages'.
--
dependencyGraph :: (PackageFixedDeps pkg, HasComponentId pkg)
                => FakeMap
                -> PackageIndex pkg
                -> (Graph.Graph,
                    Graph.Vertex -> pkg,
                    ComponentId -> Maybe Graph.Vertex)
dependencyGraph fakeMap index = (graph, vertexToPkg, idToVertex)
  where
    (graph, vertexToPkg', idToVertex) = Graph.graphFromEdges edges
    vertexToPkg = fromJust
                . (\((), key, _targets) -> lookupComponentId index key)
                . vertexToPkg'

    pkgs  = allPackages index
    edges = map edgesFrom pkgs

    resolve   pid = Map.findWithDefault pid pid fakeMap
    edgesFrom pkg = ( ()
                    , resolve (installedComponentId pkg)
                    , CD.nonSetupDeps (fakeDepends fakeMap pkg)
                    )

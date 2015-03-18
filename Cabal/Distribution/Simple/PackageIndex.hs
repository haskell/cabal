{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of packages.
--
module Distribution.Simple.PackageIndex (
  -- * Package index data type
  InstalledPackageIndex,
  PackageIndex,
  FakeMap,

  -- * Creating an index
  fromList,

  -- * Updates
  merge,

  insert,

  deleteInstalledPackageId,
  deleteSourcePackageId,
  deletePackageName,
--  deleteDependency,

  -- * Queries

  -- ** Precise lookups
  lookupInstalledPackageId,
  lookupSourcePackageId,
  lookupPackageId,
  lookupPackageName,
  lookupDependency,

  -- ** Case-insensitive searches
  searchByName,
  SearchResult(..),
  searchByNameSubstring,

  -- ** Bulk queries
  allPackages,
  allPackagesByName,
  allPackagesBySourcePackageId,

  -- ** Special queries
  brokenPackages,
  dependencyClosure,
  reverseDependencyClosure,
  topologicalOrder,
  reverseTopologicalOrder,
  dependencyInconsistencies,
  dependencyCycles,
  dependencyGraph,
  moduleNameIndex,

  -- ** Variants of special queries supporting fake map
  fakeLookupInstalledPackageId,
  brokenPackages',
  dependencyClosure',
  reverseDependencyClosure',
  dependencyInconsistencies',
  dependencyCycles',
  dependencyGraph',
  ) where

import Control.Exception (assert)
import Data.Array ((!))
import qualified Data.Array as Array
import Distribution.Compat.Binary (Binary)
import qualified Data.Graph as Graph
import Data.List as List
         ( null, foldl', sort
         , groupBy, sortBy, find, isInfixOf, nubBy, deleteBy, deleteFirstsBy )
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(..))
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.Tree  as Tree
import GHC.Generics (Generic)
import Prelude hiding (lookup)

import Distribution.Package
         ( PackageName(..), PackageId
         , Package(..), packageName, packageVersion
         , Dependency(Dependency)--, --PackageFixedDeps(..)
         , InstalledPackageId(..), PackageInstalled(..) )
import Distribution.ModuleName
         ( ModuleName )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Version
         ( Version, withinRange )
import Distribution.Simple.Utils (lowercase, comparing, equating)

-- Note [FakeMap]
-----------------
-- We'd like to use the PackageIndex defined in this module for
-- cabal-install's InstallPlan.  However, at the moment, this
-- data structure is indexed by InstalledPackageId, which we don't
-- know until after we've compiled a package (whereas InstallPlan
-- needs to store not-compiled packages in the index.) Eventually,
-- an InstalledPackageId will be calculatable prior to actually
-- building the package (making it something of a misnomer), but
-- at the moment, the "fake installed package ID map" is a workaround
-- to solve this problem while reusing PackageIndex.  The basic idea
-- is that, since we don't know what an InstalledPackageId is
-- beforehand, we just fake up one based on the package ID (it only
-- needs to be unique for the particular install plan), and fill
-- it out with the actual generated InstalledPackageId after the
-- package is successfully compiled.
--
-- However, there is a problem: in the index there may be
-- references using the old package ID, which are now dangling if
-- we update the InstalledPackageId.  We could map over the entire
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

-- | Map from fake installed package IDs to real ones.  See Note [FakeMap]
type FakeMap = Map InstalledPackageId InstalledPackageId

-- | The collection of information about packages from one or more 'PackageDB's.
-- These packages generally should have an instance of 'PackageInstalled'
--
-- Packages are uniquely identified in by their 'InstalledPackageId', they can
-- also be efficiently looked up by package name or by name and version.
--
data PackageIndex a = PackageIndex
  -- The primary index. Each InstalledPackageInfo record is uniquely identified
  -- by its InstalledPackageId.
  --
  !(Map InstalledPackageId a)

  -- This auxiliary index maps package names (case-sensitively) to all the
  -- versions and instances of that package. This allows us to find all
  -- versions satisfying a dependency.
  --
  -- It is a three-level index. The first level is the package name,
  -- the second is the package version and the final level is instances
  -- of the same package version. These are unique by InstalledPackageId
  -- and are kept in preference order.
  --
  -- FIXME: Clarify what "preference order" means. Check that this invariant is
  -- preserved. See #1463 for discussion.
  !(Map PackageName (Map Version [a]))

  deriving (Generic, Show, Read)

instance Binary a => Binary (PackageIndex a)

-- | The default package index which contains 'InstalledPackageInfo'.  Normally
-- use this.
type InstalledPackageIndex = PackageIndex InstalledPackageInfo

instance PackageInstalled a => Monoid (PackageIndex a) where
  mempty  = PackageIndex Map.empty Map.empty
  mappend = merge
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

invariant :: PackageInstalled a => PackageIndex a -> Bool
invariant (PackageIndex pids pnames) =
     map installedPackageId (Map.elems pids)
  == sort
     [ assert pinstOk (installedPackageId pinst)
     | (pname, pvers)  <- Map.toList pnames
     , let pversOk = not (Map.null pvers)
     , (pver,  pinsts) <- assert pversOk $ Map.toList pvers
     , let pinsts'  = sortBy (comparing installedPackageId) pinsts
           pinstsOk = all (\g -> length g == 1)
                          (groupBy (equating installedPackageId) pinsts')
     , pinst           <- assert pinstsOk $ pinsts'
     , let pinstOk = packageName    pinst == pname
                  && packageVersion pinst == pver
     ]


--
-- * Internal helpers
--

mkPackageIndex :: PackageInstalled a
               => Map InstalledPackageId a
               -> Map PackageName (Map Version [a])
               -> PackageIndex a
mkPackageIndex pids pnames = assert (invariant index) index
  where index = PackageIndex pids pnames


--
-- * Construction
--

-- | Build an index out of a bunch of packages.
--
-- If there are duplicates by 'InstalledPackageId' then later ones mask earlier
-- ones.
--
fromList :: PackageInstalled a => [a] -> PackageIndex a
fromList pkgs = mkPackageIndex pids pnames
  where
    pids      = Map.fromList [ (installedPackageId pkg, pkg) | pkg <- pkgs ]
    pnames    =
      Map.fromList
        [ (packageName (head pkgsN), pvers)
        | pkgsN <- groupBy (equating  packageName)
                 . sortBy  (comparing packageId)
                 $ pkgs
        , let pvers =
                Map.fromList
                [ (packageVersion (head pkgsNV),
                   nubBy (equating installedPackageId) (reverse pkgsNV))
                | pkgsNV <- groupBy (equating packageVersion) pkgsN
                ]
        ]

--
-- * Updates
--

-- | Merge two indexes.
--
-- Packages from the second mask packages from the first if they have the exact
-- same 'InstalledPackageId'.
--
-- For packages with the same source 'PackageId', packages from the second are
-- \"preferred\" over those from the first. Being preferred means they are top
-- result when we do a lookup by source 'PackageId'. This is the mechanism we
-- use to prefer user packages over global packages.
--
merge :: PackageInstalled a => PackageIndex a -> PackageIndex a -> PackageIndex a
merge (PackageIndex pids1 pnames1) (PackageIndex pids2 pnames2) =
  mkPackageIndex (Map.unionWith (\_ y -> y) pids1 pids2)
                 (Map.unionWith (Map.unionWith mergeBuckets) pnames1 pnames2)
  where
    -- Packages in the second list mask those in the first, however preferred
    -- packages go first in the list.
    mergeBuckets xs ys = ys ++ (xs \\ ys)
    (\\) = deleteFirstsBy (equating installedPackageId)


-- | Inserts a single package into the index.
--
-- This is equivalent to (but slightly quicker than) using 'mappend' or
-- 'merge' with a singleton index.
--
insert :: PackageInstalled a => a -> PackageIndex a -> PackageIndex a
insert pkg (PackageIndex pids pnames) =
    mkPackageIndex pids' pnames'

  where
    pids'   = Map.insert (installedPackageId pkg) pkg pids
    pnames' = insertPackageName pnames
    insertPackageName =
      Map.insertWith' (\_ -> insertPackageVersion)
                     (packageName pkg)
                     (Map.singleton (packageVersion pkg) [pkg])

    insertPackageVersion =
      Map.insertWith' (\_ -> insertPackageInstance)
                     (packageVersion pkg) [pkg]

    insertPackageInstance pkgs =
      pkg : deleteBy (equating installedPackageId) pkg pkgs


-- | Removes a single installed package from the index.
--
deleteInstalledPackageId :: PackageInstalled a => InstalledPackageId -> PackageIndex a -> PackageIndex a
deleteInstalledPackageId ipkgid original@(PackageIndex pids pnames) =
  case Map.updateLookupWithKey (\_ _ -> Nothing) ipkgid pids of
    (Nothing,     _)     -> original
    (Just spkgid, pids') -> mkPackageIndex pids'
                                          (deletePkgName spkgid pnames)

  where
    deletePkgName spkgid =
      Map.update (deletePkgVersion spkgid) (packageName spkgid)

    deletePkgVersion spkgid =
        (\m -> if Map.null m then Nothing else Just m)
      . Map.update deletePkgInstance (packageVersion spkgid)

    deletePkgInstance =
        (\xs -> if List.null xs then Nothing else Just xs)
      . List.deleteBy (\_ pkg -> installedPackageId pkg == ipkgid) undefined


-- | Removes all packages with this source 'PackageId' from the index.
--
deleteSourcePackageId :: PackageInstalled a => PackageId -> PackageIndex a -> PackageIndex a
deleteSourcePackageId pkgid original@(PackageIndex pids pnames) =
  case Map.lookup (packageName pkgid) pnames of
    Nothing     -> original
    Just pvers  -> case Map.lookup (packageVersion pkgid) pvers of
      Nothing   -> original
      Just pkgs -> mkPackageIndex
                     (foldl' (flip (Map.delete . installedPackageId)) pids pkgs)
                     (deletePkgName pnames)
  where
    deletePkgName =
      Map.update deletePkgVersion (packageName pkgid)

    deletePkgVersion =
        (\m -> if Map.null m then Nothing else Just m)
      . Map.delete (packageVersion pkgid)


-- | Removes all packages with this (case-sensitive) name from the index.
--
deletePackageName :: PackageInstalled a => PackageName -> PackageIndex a -> PackageIndex a
deletePackageName name original@(PackageIndex pids pnames) =
  case Map.lookup name pnames of
    Nothing     -> original
    Just pvers  -> mkPackageIndex
                     (foldl' (flip (Map.delete . installedPackageId)) pids
                             (concat (Map.elems pvers)))
                     (Map.delete name pnames)

{-
-- | Removes all packages satisfying this dependency from the index.
--
deleteDependency :: Dependency -> PackageIndex -> PackageIndex
deleteDependency (Dependency name verstionRange) =
  delete' name (\pkg -> packageVersion pkg `withinRange` verstionRange)
-}

--
-- * Bulk queries
--

-- | Get all the packages from the index.
--
allPackages :: PackageIndex a -> [a]
allPackages (PackageIndex pids _) = Map.elems pids

-- | Get all the packages from the index.
--
-- They are grouped by package name (case-sensitively).
--
allPackagesByName :: PackageIndex a -> [(PackageName, [a])]
allPackagesByName (PackageIndex _ pnames) =
  [ (pkgname, concat (Map.elems pvers))
  | (pkgname, pvers) <- Map.toList pnames ]

-- | Get all the packages from the index.
--
-- They are grouped by source package id (package name and version).
--
allPackagesBySourcePackageId :: PackageInstalled a => PackageIndex a -> [(PackageId, [a])]
allPackagesBySourcePackageId (PackageIndex _ pnames) =
  [ (packageId ipkg, ipkgs)
  | pvers <- Map.elems pnames
  , ipkgs@(ipkg:_) <- Map.elems pvers ]

--
-- * Lookups
--

-- | Does a lookup by source package id (name & version).
--
-- Since multiple package DBs mask each other by 'InstalledPackageId',
-- then we get back at most one package.
--
lookupInstalledPackageId :: PackageInstalled a => PackageIndex a -> InstalledPackageId
                         -> Maybe a
lookupInstalledPackageId (PackageIndex pids _) pid = Map.lookup pid pids


-- | Does a lookup by source package id (name & version).
--
-- There can be multiple installed packages with the same source 'PackageId'
-- but different 'InstalledPackageId'. They are returned in order of
-- preference, with the most preferred first.
--
lookupSourcePackageId :: PackageInstalled a => PackageIndex a -> PackageId -> [a]
lookupSourcePackageId (PackageIndex _ pnames) pkgid =
  case Map.lookup (packageName pkgid) pnames of
    Nothing     -> []
    Just pvers  -> case Map.lookup (packageVersion pkgid) pvers of
      Nothing   -> []
      Just pkgs -> pkgs -- in preference order

-- | Convenient alias of 'lookupSourcePackageId', but assuming only
-- one package per package ID.
lookupPackageId :: PackageInstalled a => PackageIndex a -> PackageId -> Maybe a
lookupPackageId index pkgid = case lookupSourcePackageId index pkgid  of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> error "Distribution.Simple.PackageIndex: multiple matches found"

-- | Does a lookup by source package name.
--
lookupPackageName :: PackageInstalled a => PackageIndex a -> PackageName
                  -> [(Version, [a])]
lookupPackageName (PackageIndex _ pnames) name =
  case Map.lookup name pnames of
    Nothing     -> []
    Just pvers  -> Map.toList pvers


-- | Does a lookup by source package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: PackageInstalled a => PackageIndex a -> Dependency
                 -> [(Version, [a])]
lookupDependency (PackageIndex _ pnames) (Dependency name versionRange) =
  case Map.lookup name pnames of
    Nothing    -> []
    Just pvers -> [ entry
                  | entry@(ver, _) <- Map.toList pvers
                  , ver `withinRange` versionRange ]

--
-- * Case insensitive name lookups
--

-- | Does a case-insensitive search by package name.
--
-- If there is only one package that compares case-insensitively to this name
-- then the search is unambiguous and we get back all versions of that package.
-- If several match case-insensitively but one matches exactly then it is also
-- unambiguous.
--
-- If however several match case-insensitively and none match exactly then we
-- have an ambiguous result, and we get back all the versions of all the
-- packages. The list of ambiguous results is split by exact package name. So
-- it is a non-empty list of non-empty lists.
--
searchByName :: PackageInstalled a => PackageIndex a -> String -> SearchResult [a]
searchByName (PackageIndex _ pnames) name =
  case [ pkgs | pkgs@(PackageName name',_) <- Map.toList pnames
              , lowercase name' == lname ] of
    []               -> None
    [(_,pvers)]      -> Unambiguous (concat (Map.elems pvers))
    pkgss            -> case find ((PackageName name==) . fst) pkgss of
      Just (_,pvers) -> Unambiguous (concat (Map.elems pvers))
      Nothing        -> Ambiguous (map (concat . Map.elems . snd) pkgss)
  where lname = lowercase name

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: PackageInstalled a => PackageIndex a -> String -> [a]
searchByNameSubstring (PackageIndex _ pnames) searchterm =
  [ pkg
  | (PackageName name, pvers) <- Map.toList pnames
  , lsearchterm `isInfixOf` lowercase name
  , pkgs <- Map.elems pvers
  , pkg <- pkgs ]
  where lsearchterm = lowercase searchterm


--
-- * Special queries
--

-- None of the stuff below depends on the internal representation of the index.
--

-- | Find if there are any cycles in the dependency graph. If there are no
-- cycles the result is @[]@.
--
-- This actually computes the strongly connected components. So it gives us a
-- list of groups of packages where within each group they all depend on each
-- other, directly or indirectly.
--
dependencyCycles :: PackageInstalled a => PackageIndex a -> [[a]]
dependencyCycles = dependencyCycles' Map.empty

-- | Variant of 'dependencyCycles' which accepts a 'FakeMap'.  See Note [FakeMap].
dependencyCycles' :: PackageInstalled a => FakeMap -> PackageIndex a -> [[a]]
dependencyCycles' fakeMap index =
  [ vs | Graph.CyclicSCC vs <- Graph.stronglyConnComp adjacencyList ]
  where
    adjacencyList = [ (pkg, installedPackageId pkg, fakeInstalledDepends fakeMap pkg)
                    | pkg <- allPackages index ]


-- | All packages that have immediate dependencies that are not in the index.
--
-- Returns such packages along with the dependencies that they're missing.
--
brokenPackages :: PackageInstalled a => PackageIndex a -> [(a, [InstalledPackageId])]
brokenPackages = brokenPackages' Map.empty

-- | Variant of 'brokenPackages' which accepts a 'FakeMap'.  See Note [FakeMap].
brokenPackages' :: PackageInstalled a => FakeMap -> PackageIndex a -> [(a, [InstalledPackageId])]
brokenPackages' fakeMap index =
  [ (pkg, missing)
  | pkg  <- allPackages index
  , let missing = [ pkg' | pkg' <- installedDepends pkg
                         , isNothing (fakeLookupInstalledPackageId fakeMap index pkg') ]
  , not (null missing) ]

-- | Variant of 'lookupInstalledPackageId' which accepts a 'FakeMap'.  See Note [FakeMap].
fakeLookupInstalledPackageId :: PackageInstalled a => FakeMap -> PackageIndex a -> InstalledPackageId -> Maybe a
fakeLookupInstalledPackageId fakeMap index pkg = lookupInstalledPackageId index (Map.findWithDefault pkg pkg fakeMap)

-- | Tries to take the transitive closure of the package dependencies.
--
-- If the transitive closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if the result is @Right []@ it is because at least one of
-- the original given 'PackageId's do not occur in the index.
--
dependencyClosure :: PackageInstalled a => PackageIndex a
                  -> [InstalledPackageId]
                  -> Either (PackageIndex a)
                            [(a, [InstalledPackageId])]
dependencyClosure = dependencyClosure' Map.empty

-- | Variant of 'dependencyClosure' which accepts a 'FakeMap'.  See Note [FakeMap].
dependencyClosure' :: PackageInstalled a => FakeMap
                  -> PackageIndex a
                  -> [InstalledPackageId]
                  -> Either (PackageIndex a)
                            [(a, [InstalledPackageId])]
dependencyClosure' fakeMap index pkgids0 = case closure mempty [] pkgids0 of
  (completed, []) -> Left completed
  (completed, _)  -> Right (brokenPackages completed)
 where
    closure completed failed []             = (completed, failed)
    closure completed failed (pkgid:pkgids) = case fakeLookupInstalledPackageId fakeMap index pkgid of
      Nothing   -> closure completed (pkgid:failed) pkgids
      Just pkg  -> case fakeLookupInstalledPackageId fakeMap completed (installedPackageId pkg) of
        Just _  -> closure completed  failed pkgids
        Nothing -> closure completed' failed pkgids'
          where completed' = insert pkg completed
                pkgids'    = installedDepends pkg ++ pkgids

-- | Takes the transitive closure of the packages reverse dependencies.
--
-- * The given 'PackageId's must be in the index.
--
reverseDependencyClosure :: PackageInstalled a => PackageIndex a
                         -> [InstalledPackageId]
                         -> [a]
reverseDependencyClosure = reverseDependencyClosure' Map.empty

-- | Variant of 'reverseDependencyClosure' which accepts a 'FakeMap'.  See Note [FakeMap].
reverseDependencyClosure' :: PackageInstalled a => FakeMap
                         -> PackageIndex a
                         -> [InstalledPackageId]
                         -> [a]
reverseDependencyClosure' fakeMap index =
    map vertexToPkg
  . concatMap Tree.flatten
  . Graph.dfs reverseDepGraph
  . map (fromMaybe noSuchPkgId . pkgIdToVertex)

  where
    (depGraph, vertexToPkg, pkgIdToVertex) = dependencyGraph' fakeMap index
    reverseDepGraph = Graph.transposeG depGraph
    noSuchPkgId = error "reverseDependencyClosure: package is not in the graph"

topologicalOrder :: PackageInstalled a => PackageIndex a -> [a]
topologicalOrder index = map toPkgId
                       . Graph.topSort
                       $ graph
  where (graph, toPkgId, _) = dependencyGraph index

reverseTopologicalOrder :: PackageInstalled a => PackageIndex a -> [a]
reverseTopologicalOrder index = map toPkgId
                              . Graph.topSort
                              . Graph.transposeG
                              $ graph
  where (graph, toPkgId, _) = dependencyGraph index

-- | Builds a graph of the package dependencies.
--
-- Dependencies on other packages that are not in the index are discarded.
-- You can check if there are any such dependencies with 'brokenPackages'.
--
dependencyGraph :: PackageInstalled a => PackageIndex a
                -> (Graph.Graph,
                    Graph.Vertex -> a,
                    InstalledPackageId -> Maybe Graph.Vertex)
dependencyGraph = dependencyGraph' Map.empty

-- | Variant of 'dependencyGraph' which accepts a 'FakeMap'.  See Note [FakeMap].
dependencyGraph' :: PackageInstalled a => FakeMap
                -> PackageIndex a
                -> (Graph.Graph,
                    Graph.Vertex -> a,
                    InstalledPackageId -> Maybe Graph.Vertex)
dependencyGraph' fakeMap index = (graph, vertex_to_pkg, id_to_vertex)
  where
    graph = Array.listArray bounds
              [ [ v | Just v <- map id_to_vertex (installedDepends pkg) ]
              | pkg <- pkgs ]

    pkgs             = sortBy (comparing packageId) (allPackages index)
    vertices         = zip (map installedPackageId pkgs) [0..]
    vertex_map       = Map.fromList vertices
    id_to_vertex pid = Map.lookup (Map.findWithDefault pid pid fakeMap) vertex_map

    vertex_to_pkg vertex = pkgTable ! vertex

    pkgTable   = Array.listArray bounds pkgs
    topBound = length pkgs - 1
    bounds = (0, topBound)

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
dependencyInconsistencies :: PackageInstalled a => PackageIndex a
                          -> [(PackageName, [(PackageId, Version)])]
dependencyInconsistencies = dependencyInconsistencies' Map.empty

-- | Variant of 'dependencyInconsistencies' which accepts a 'FakeMap'.  See Note [FakeMap].
dependencyInconsistencies' :: PackageInstalled a => FakeMap -> PackageIndex a
                          -> [(PackageName, [(PackageId, Version)])]
dependencyInconsistencies' fakeMap index =
  [ (name, [ (pid,packageVersion dep) | (dep,pids) <- uses, pid <- pids])
  | (name, ipid_map) <- Map.toList inverseIndex
  , let uses = Map.elems ipid_map
  , reallyIsInconsistent (map fst uses) ]

  where -- for each PackageName,
        --   for each package with that name,
        --     the InstalledPackageInfo and the package Ids of packages
        --     that depend on it.
        inverseIndex = Map.fromListWith (Map.unionWith (\(a,b) (_,b') -> (a,b++b')))
          [ (packageName dep,
             Map.fromList [(ipid,(dep,[packageId pkg]))])
          | pkg <- allPackages index
          , ipid <- fakeInstalledDepends fakeMap pkg
          , Just dep <- [fakeLookupInstalledPackageId fakeMap index ipid]
          ]

        reallyIsInconsistent :: PackageInstalled a => [a] -> Bool
        reallyIsInconsistent []       = False
        reallyIsInconsistent [_p]     = False
        reallyIsInconsistent [p1, p2] =
          let pid1 = installedPackageId p1
              pid2 = installedPackageId p2
          in Map.findWithDefault pid1 pid1 fakeMap `notElem` fakeInstalledDepends fakeMap p2
          && Map.findWithDefault pid2 pid2 fakeMap `notElem` fakeInstalledDepends fakeMap p1
        reallyIsInconsistent _ = True

-- | Variant of 'installedDepends' which accepts a 'FakeMap'.  See Note [FakeMap].
fakeInstalledDepends :: PackageInstalled a => FakeMap -> a -> [InstalledPackageId]
fakeInstalledDepends fakeMap = map (\pid -> Map.findWithDefault pid pid fakeMap) . installedDepends

-- | A rough approximation of GHC's module finder, takes a 'InstalledPackageIndex' and
-- turns it into a map from module names to their source packages.  It's used to
-- initialize the @build-deps@ field in @cabal init@.
moduleNameIndex :: InstalledPackageIndex -> Map ModuleName [InstalledPackageInfo]
moduleNameIndex index =
  Map.fromListWith (++) $ do
    pkg <- allPackages index
    IPI.ExposedModule m reexport _ <- IPI.exposedModules pkg
    case reexport of
        Nothing -> return (m, [pkg])
        Just (IPI.OriginalModule _ m') | m == m'   -> []
                                       | otherwise -> return (m', [pkg])
        -- The heuristic is this: we want to prefer the original package
        -- which originally exported a module.  However, if a reexport
        -- also *renamed* the module (m /= m'), then we have to use the
        -- downstream package, since the upstream package has the wrong
        -- module name!

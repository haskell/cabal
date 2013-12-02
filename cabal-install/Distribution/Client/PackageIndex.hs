-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of packages.
--
module Distribution.Client.PackageIndex (
  -- * Package index data type
  PackageIndex,

  -- * Creating an index
  fromList,

  -- * Updates
  merge,
  insert,
  deletePackageName,
  deletePackageId,
  deleteDependency,

  -- * Queries

  -- ** Precise lookups
  elemByPackageId,
  elemByPackageName,
  lookupPackageName,
  lookupPackageId,
  lookupDependency,

  -- ** Case-insensitive searches
  searchByName,
  SearchResult(..),
  searchByNameSubstring,

  -- ** Bulk queries
  allPackages,
  allPackagesByName,

  -- ** Special queries
  brokenPackages,
  dependencyClosure,
  reverseDependencyClosure,
  topologicalOrder,
  reverseTopologicalOrder,
  dependencyInconsistencies,
  dependencyCycles,
  dependencyGraph,
  ) where

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Tree  as Tree
import qualified Data.Graph as Graph
import qualified Data.Array as Array
import Data.Array ((!))
import Data.List (groupBy, sortBy, nub, isInfixOf)
import Data.Monoid (Monoid(..))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)

import Distribution.Package
         ( PackageName(..), PackageIdentifier(..)
         , Package(..), packageName, packageVersion
         , Dependency(Dependency), PackageFixedDeps(..) )
import Distribution.Version
         ( Version, withinRange )
import Distribution.Simple.Utils (lowercase, equating, comparing)


-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched effeciently by package name and version.
--
newtype PackageIndex pkg = PackageIndex
  -- This index package names to all the package records matching that package
  -- name case-sensitively. It includes all versions.
  --
  -- This allows us to find all versions satisfying a dependency.
  -- Most queries are a map lookup followed by a linear scan of the bucket.
  --
  (Map PackageName [pkg])

  deriving (Show, Read)

instance Functor PackageIndex where
  fmap f (PackageIndex m) = PackageIndex (fmap (map f) m)

instance Package pkg => Monoid (PackageIndex pkg) where
  mempty  = PackageIndex Map.empty
  mappend = merge
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

invariant :: Package pkg => PackageIndex pkg -> Bool
invariant (PackageIndex m) = all (uncurry goodBucket) (Map.toList m)
  where
    goodBucket _    [] = False
    goodBucket name (pkg0:pkgs0) = check (packageId pkg0) pkgs0
      where
        check pkgid []          = packageName pkgid == name
        check pkgid (pkg':pkgs) = packageName pkgid == name
                               && pkgid < pkgid'
                               && check pkgid' pkgs
          where pkgid' = packageId pkg'

--
-- * Internal helpers
--

mkPackageIndex :: Package pkg => Map PackageName [pkg] -> PackageIndex pkg
mkPackageIndex index = assert (invariant (PackageIndex index))
                                         (PackageIndex index)

internalError :: String -> a
internalError name = error ("PackageIndex." ++ name ++ ": internal error")

-- | Lookup a name in the index to get all packages that match that name
-- case-sensitively.
--
lookup :: Package pkg => PackageIndex pkg -> PackageName -> [pkg]
lookup (PackageIndex m) name = fromMaybe [] $ Map.lookup name m

--
-- * Construction
--

-- | Build an index out of a bunch of packages.
--
-- If there are duplicates, later ones mask earlier ones.
--
fromList :: Package pkg => [pkg] -> PackageIndex pkg
fromList pkgs = mkPackageIndex
              . Map.map fixBucket
              . Map.fromListWith (++)
              $ [ (packageName pkg, [pkg])
                | pkg <- pkgs ]
  where
    fixBucket = -- out of groups of duplicates, later ones mask earlier ones
                -- but Map.fromListWith (++) constructs groups in reverse order
                map head
                -- Eq instance for PackageIdentifier is wrong, so use Ord:
              . groupBy (\a b -> EQ == comparing packageId a b)
                -- relies on sortBy being a stable sort so we
                -- can pick consistently among duplicates
              . sortBy (comparing packageId)

--
-- * Updates
--

-- | Merge two indexes.
--
-- Packages from the second mask packages of the same exact name
-- (case-sensitively) from the first.
--
merge :: Package pkg => PackageIndex pkg -> PackageIndex pkg -> PackageIndex pkg
merge i1@(PackageIndex m1) i2@(PackageIndex m2) =
  assert (invariant i1 && invariant i2) $
    mkPackageIndex (Map.unionWith mergeBuckets m1 m2)

-- | Elements in the second list mask those in the first.
mergeBuckets :: Package pkg => [pkg] -> [pkg] -> [pkg]
mergeBuckets []     ys     = ys
mergeBuckets xs     []     = xs
mergeBuckets xs@(x:xs') ys@(y:ys') =
      case packageId x `compare` packageId y of
        GT -> y : mergeBuckets xs  ys'
        EQ -> y : mergeBuckets xs' ys'
        LT -> x : mergeBuckets xs' ys

-- | Inserts a single package into the index.
--
-- This is equivalent to (but slightly quicker than) using 'mappend' or
-- 'merge' with a singleton index.
--
insert :: Package pkg => pkg -> PackageIndex pkg -> PackageIndex pkg
insert pkg (PackageIndex index) = mkPackageIndex $
  Map.insertWith (\_ -> insertNoDup) (packageName pkg) [pkg] index
  where
    pkgid = packageId pkg
    insertNoDup []                = [pkg]
    insertNoDup pkgs@(pkg':pkgs') = case compare pkgid (packageId pkg') of
      LT -> pkg  : pkgs
      EQ -> pkg  : pkgs'
      GT -> pkg' : insertNoDup pkgs'

-- | Internal delete helper.
--
delete :: Package pkg => PackageName -> (pkg -> Bool) -> PackageIndex pkg -> PackageIndex pkg
delete name p (PackageIndex index) = mkPackageIndex $
  Map.update filterBucket name index
  where
    filterBucket = deleteEmptyBucket
                 . filter (not . p)
    deleteEmptyBucket []        = Nothing
    deleteEmptyBucket remaining = Just remaining

-- | Removes a single package from the index.
--
deletePackageId :: Package pkg => PackageIdentifier -> PackageIndex pkg -> PackageIndex pkg
deletePackageId pkgid =
  delete (packageName pkgid) (\pkg -> packageId pkg == pkgid)

-- | Removes all packages with this (case-sensitive) name from the index.
--
deletePackageName :: Package pkg => PackageName -> PackageIndex pkg -> PackageIndex pkg
deletePackageName name =
  delete name (\pkg -> packageName pkg == name)

-- | Removes all packages satisfying this dependency from the index.
--
deleteDependency :: Package pkg => Dependency -> PackageIndex pkg -> PackageIndex pkg
deleteDependency (Dependency name verstionRange) =
  delete name (\pkg -> packageVersion pkg `withinRange` verstionRange)

--
-- * Bulk queries
--

-- | Get all the packages from the index.
--
allPackages :: Package pkg => PackageIndex pkg -> [pkg]
allPackages (PackageIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: Package pkg => PackageIndex pkg -> [[pkg]]
allPackagesByName (PackageIndex m) = Map.elems m

--
-- * Lookups
--

elemByPackageId :: Package pkg => PackageIndex pkg -> PackageIdentifier -> Bool
elemByPackageId index = isJust . lookupPackageId index

elemByPackageName :: Package pkg => PackageIndex pkg -> PackageName -> Bool
elemByPackageName index = not . null . lookupPackageName index


-- | Does a lookup by package id (name & version).
--
-- Since multiple package DBs mask each other case-sensitively by package name,
-- then we get back at most one package.
--
lookupPackageId :: Package pkg => PackageIndex pkg -> PackageIdentifier -> Maybe pkg
lookupPackageId index pkgid =
  case [ pkg | pkg <- lookup index (packageName pkgid)
             , packageId pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name.
--
lookupPackageName :: Package pkg => PackageIndex pkg -> PackageName -> [pkg]
lookupPackageName index name =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name ]

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: Package pkg => PackageIndex pkg -> Dependency -> [pkg]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name
        , packageVersion pkg `withinRange` versionRange ]

--
-- * Case insensitive name lookups
--

-- | Does a case-insensitive search by package name.
--
-- If there is only one package that compares case-insentiviely to this name
-- then the search is unambiguous and we get back all versions of that package.
-- If several match case-insentiviely but one matches exactly then it is also
-- unambiguous.
--
-- If however several match case-insentiviely and none match exactly then we
-- have an ambiguous result, and we get back all the versions of all the
-- packages. The list of ambiguous results is split by exact package name. So
-- it is a non-empty list of non-empty lists.
--
searchByName :: Package pkg => PackageIndex pkg
             -> String -> [(PackageName, [pkg])]
searchByName (PackageIndex m) name =
    [ pkgs
    | pkgs@(PackageName name',_) <- Map.toList m
    , lowercase name' == lname ]
  where
    lname = lowercase name

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: Package pkg => PackageIndex pkg
                      -> String -> [(PackageName, [pkg])]
searchByNameSubstring (PackageIndex m) searchterm =
    [ pkgs
    | pkgs@(PackageName name, _) <- Map.toList m
    , lsearchterm `isInfixOf` lowercase name ]
  where
    lsearchterm = lowercase searchterm

--
-- * Special queries
--

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

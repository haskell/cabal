{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- An index of packages.
--
module Distribution.Simple.PackageIndex (
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

  -- * The index of installed packages
  InstalledPackageIndex,
  listToInstalledPackageIndex,
  lookupInstalledPackageByName,
  addToInstalledPackageIndex,
  lookupInstalledPackage,
  allInstalledPackages

  ) where

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Tree  as Tree
import qualified Data.Graph as Graph
import qualified Data.Array as Array
import Data.Array ((!))
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
import Data.List (groupBy, sortBy, nub, find, isPrefixOf, tails)
#else
import Data.List (groupBy, sortBy, find, isInfixOf)
#endif
import Data.Monoid (Monoid(..))
import Data.Maybe (isNothing, fromMaybe)

import Distribution.Package
         ( PackageName(..), PackageIdentifier(..)
         , Package(..), packageName, packageVersion
         , Dependency(Dependency), PackageFixedDeps(..)
         , InstalledPackageId(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, installedPackageId, sourcePackageId )
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Version
         ( Version, withinRange )
import Distribution.Simple.Utils (lowercase, comparing)

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
import Text.Read
import qualified Text.Read.Lex as L
#endif

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
#endif

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched effeciently by package name and version.
--
newtype Package pkg => PackageIndex pkg = PackageIndex
  -- This index package names to all the package records matching that package
  -- name case-sensitively. It includes all versions.
  --
  -- This allows us to find all versions satisfying a dependency.
  -- Most queries are a map lookup followed by a linear scan of the bucket.
  --
  (Map PackageName [pkg])

#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 606)
  deriving (Show, Read)
#else
-- The Show/Read instance for Data.Map in ghc-6.4 is useless
-- so we have to re-implement it here:
instance (Package pkg, Show pkg) => Show (PackageIndex pkg) where
  showsPrec d (PackageIndex m) =
      showParen (d > 10) (showString "PackageIndex" . shows (Map.toList m))

instance (Package pkg, Read pkg) => Read (PackageIndex pkg) where
  readPrec = parens $ prec 10 $ do
    Ident "PackageIndex" <- lexP
    xs <- readPrec
    return (PackageIndex (Map.fromList xs))
      where parens :: ReadPrec a -> ReadPrec a
            parens p = optional
             where
               optional  = p +++ mandatory
               mandatory = paren optional

            paren :: ReadPrec a -> ReadPrec a
            paren p = do L.Punc "(" <- lexP
                         x          <- reset p
                         L.Punc ")" <- lexP
                         return x

  readListPrec = readListPrecDefault
#endif

instance Package pkg => Monoid (PackageIndex pkg) where
  mempty  = PackageIndex (Map.empty)
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
searchByName :: Package pkg => PackageIndex pkg -> String -> SearchResult [pkg]
searchByName (PackageIndex m) name =
  case [ pkgs | pkgs@(PackageName name',_) <- Map.toList m
              , lowercase name' == lname ] of
    []              -> None
    [(_,pkgs)]      -> Unambiguous pkgs
    pkgss           -> case find ((PackageName name==) . fst) pkgss of
      Just (_,pkgs) -> Unambiguous pkgs
      Nothing       -> Ambiguous (map snd pkgss)
  where lname = lowercase name

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: Package pkg => PackageIndex pkg -> String -> [pkg]
searchByNameSubstring (PackageIndex m) searchterm =
  [ pkg
  | (PackageName name, pkgs) <- Map.toList m
  , lsearchterm `isInfixOf` lowercase name
  , pkg <- pkgs ]
  where lsearchterm = lowercase searchterm

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

-----------------------------------------------------------------------------
-- The Installed Package index
-----------------------------------------------------------------------------

-- | This is a mapping from 'InstalledPackageId' to 'InstalledPackageInfo'.
-- Since an 'InstalledPackageId' uniquely identifies a package, there
-- is a single 'InstalledPackageInfo' for each 'InstalledPackageId'.
newtype InstalledPackageIndex
      = InstalledPackageIndex (Map InstalledPackageId InstalledPackageInfo)
#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 606)
  deriving (Show, Read)
#else
#error Todo: Show instance for InstalledPackageIndex
#endif

instance Monoid InstalledPackageIndex where
  mempty  = InstalledPackageIndex Map.empty
  mappend (InstalledPackageIndex ix1) (InstalledPackageIndex ix2) = 
    InstalledPackageIndex (ix1 `Map.union` ix2)

listToInstalledPackageIndex :: [InstalledPackageInfo] -> InstalledPackageIndex
listToInstalledPackageIndex ipis = 
  InstalledPackageIndex $ Map.fromList $
      [ (installedPackageId p, p) | p <- ipis ]

addToInstalledPackageIndex
   :: InstalledPackageInfo -> InstalledPackageIndex -> InstalledPackageIndex
addToInstalledPackageIndex info (InstalledPackageIndex ix)
  = InstalledPackageIndex (Map.insert (installedPackageId info) info ix)

lookupInstalledPackage :: InstalledPackageIndex -> InstalledPackageId 
                       -> Maybe InstalledPackageInfo
lookupInstalledPackage (InstalledPackageIndex ix) ipid = Map.lookup ipid ix


lookupInstalledPackageByName :: InstalledPackageIndex -> PackageName
                             -> [InstalledPackageInfo]
lookupInstalledPackageByName ix name = 
  filter ((== name) . packageName . sourcePackageId) (allInstalledPackages ix)

allInstalledPackages :: InstalledPackageIndex -> [InstalledPackageInfo]
allInstalledPackages (InstalledPackageIndex ix) = Map.elems ix

--
-- * Special queries
--

-- | All packages that have dependencies that are not in the index.
--
-- Returns such packages along with the dependencies that they're missing.
--
brokenPackages :: InstalledPackageIndex
               -> [(InstalledPackageInfo, [InstalledPackageId])]
brokenPackages index =
  [ (pkg, missing)
  | pkg  <- allInstalledPackages index
  , let missing = [ pkg' | pkg' <- IPI.depends pkg
                         , isNothing (lookupInstalledPackage index pkg') ]
  , not (null missing) ]


-- | Tries to take the transative closure of the package dependencies.
--
-- If the transative closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if the result is @Right []@ it is because at least one of
-- the original given 'PackageIdentifier's do not occur in the index.
--
dependencyClosure :: InstalledPackageIndex
                  -> [InstalledPackageId]
                  -> Either InstalledPackageIndex
                            [(InstalledPackageInfo, [InstalledPackageId])]
dependencyClosure index pkgids0 = case closure mempty [] pkgids0 of
  (completed, []) -> Left completed
  (completed, _)  -> Right (brokenPackages completed)
  where
    closure completed failed []             = (completed, failed)
    closure completed failed (pkgid:pkgids) = case lookupInstalledPackage index pkgid of
      Nothing   -> closure completed (pkgid:failed) pkgids
      Just pkg  -> case lookupInstalledPackage completed (installedPackageId pkg) of
        Just _  -> closure completed  failed pkgids
        Nothing -> closure completed' failed pkgids'
          where completed' = addToInstalledPackageIndex pkg completed
                pkgids'    = IPI.depends pkg ++ pkgids

-- | Takes the transative closure of the packages reverse dependencies.
--
-- * The given 'PackageIdentifier's must be in the index.
--
reverseDependencyClosure :: InstalledPackageIndex
                         -> [InstalledPackageId]
                         -> [InstalledPackageInfo]
reverseDependencyClosure index =
    map vertexToPkg
  . concatMap Tree.flatten
  . Graph.dfs reverseDepGraph
  . map (fromMaybe noSuchPkgId . pkgIdToVertex)

  where
    (depGraph, vertexToPkg, pkgIdToVertex) = dependencyGraph index
    reverseDepGraph = Graph.transposeG depGraph
    noSuchPkgId = error "reverseDependencyClosure: package is not in the graph"

topologicalOrder :: InstalledPackageIndex -> [InstalledPackageInfo]
topologicalOrder index = map toPkgId
                       . Graph.topSort
                       $ graph
  where (graph, toPkgId, _) = dependencyGraph index

reverseTopologicalOrder :: InstalledPackageIndex -> [InstalledPackageInfo]
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
dependencyGraph :: InstalledPackageIndex
                -> (Graph.Graph,
                    Graph.Vertex -> InstalledPackageInfo,
                    InstalledPackageId -> Maybe Graph.Vertex)
dependencyGraph index = (graph, vertex_to_pkg, id_to_vertex)
  where
    graph = Array.listArray bounds
              [ [ v | Just v <- map id_to_vertex (IPI.depends pkg) ]
              | pkg <- pkgs ]

    pkgs             = sortBy (comparing packageId) (allInstalledPackages index)
    vertices         = zip (map installedPackageId pkgs) [0..]
    vertex_map       = Map.fromList vertices
    id_to_vertex pid = Map.lookup pid vertex_map

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
dependencyInconsistencies :: InstalledPackageIndex
                          -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies index =
  [ (name, [ (pid,packageVersion dep) | (dep,pids) <- uses, pid <- pids])
  | (name, ipid_map) <- Map.toList inverseIndex
  , let uses = Map.elems ipid_map
  , reallyIsInconsistent (map fst uses) ]

  where -- for each PackageName,
        --   for each package with that name,
        --     the InstalledPackageInfo and the package Ids of packages
        --     that depend on it.
        inverseIndex :: Map PackageName
                            (Map InstalledPackageId
                                 (InstalledPackageInfo, [PackageIdentifier]))
        inverseIndex = Map.fromListWith (Map.unionWith (\(a,b) (_,b') -> (a,b++b')))
          [ (packageName dep,
             Map.fromList [(ipid,(dep,[packageId pkg]))])
          | pkg <- allInstalledPackages index
          , ipid <- IPI.depends pkg
          , Just dep <- [lookupInstalledPackage index ipid]
          ]

        reallyIsInconsistent :: [InstalledPackageInfo] -> Bool
        reallyIsInconsistent []       = False
        reallyIsInconsistent [_p]     = False
        reallyIsInconsistent [p1, p2] =
             installedPackageId p1 `notElem` IPI.depends p2
          && installedPackageId p2 `notElem` IPI.depends p1
        reallyIsInconsistent _ = True

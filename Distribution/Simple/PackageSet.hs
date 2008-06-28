{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PackageSet
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- A set of packages.
--
module Distribution.Simple.PackageSet (
  -- * Package set data type
  PackageSet,

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

  -- ** Bulk queries
  allPackages,
  allPackagesByName,

  -- ** Special queries
  brokenPackages,
  dependencyClosure,
  reverseDependencyClosure,
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
import Data.List (groupBy, sortBy)
import Data.Monoid (Monoid(..))
import Data.Maybe (isNothing, fromMaybe)

import Distribution.Package
         ( PackageName, PackageIdentifier
         , Package(..), packageName, packageVersion
         , Dependency(Dependency), PackageFixedDeps(..) )
import Distribution.Version
         ( Version, withinRange )
import Distribution.Simple.Utils (equating, comparing)

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
import Text.Read
import qualified Text.Read.Lex as L
#endif

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched effeciently by package name and version.
--
data Package pkg => PackageSet pkg = PackageSet
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
instance (Package pkg, Show pkg) => Show (PackageSet pkg) where
  showsPrec d (PackageSet m) =
      showParen (d > 10) (showString "PackageSet" . shows (Map.toList m))

instance (Package pkg, Read pkg) => Read (PackageSet pkg) where
  readPrec = parens $ prec 10 $ do
    Ident "PackageSet" <- lexP
    xs <- readPrec
    return (PackageSet (Map.fromList xs))
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

instance Package pkg => Monoid (PackageSet pkg) where
  mempty  = PackageSet (Map.empty)
  mappend = merge
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

invariant :: Package pkg => PackageSet pkg -> Bool
invariant (PackageSet m) = all (uncurry goodBucket) (Map.toList m)
  where
    goodBucket _    [] = False
    goodBucket name (pkg0:pkgs0) = check (packageId pkg0) pkgs0
      where
        check pkgid []          = packageName pkgid == name
        check pkgid (pkg':pkgs) = packageName pkgid == name
                               && pkgid < pkgid'
                               && check pkgid' pkgs
          where pkgid' = packageId pkg'

mkPackageSet :: Package pkg => Map PackageName [pkg] -> PackageSet pkg
mkPackageSet index = assert (invariant (PackageSet index))
                                       (PackageSet index)

internalError :: String -> a
internalError name = error ("PackageSet." ++ name ++ ": internal error")

-- | Lookup a name in the index to get all packages that match that name
-- case-sensitively.
--
lookup :: Package pkg => PackageSet pkg -> PackageName -> [pkg]
lookup (PackageSet m) name = fromMaybe [] $ Map.lookup name m

-- | Build a package set out of a bunch of packages.
--
-- If there are duplicates, later ones mask earlier ones.
--
fromList :: Package pkg => [pkg] -> PackageSet pkg
fromList pkgs = mkPackageSet
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

-- | Merge two indexes.
--
-- Packages from the second mask packages of the same exact name
-- (case-sensitively) from the first.
--
merge :: Package pkg => PackageSet pkg -> PackageSet pkg -> PackageSet pkg
merge i1@(PackageSet m1) i2@(PackageSet m2) =
  assert (invariant i1 && invariant i2) $
    mkPackageSet (Map.unionWith mergeBuckets m1 m2)

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
insert :: Package pkg => pkg -> PackageSet pkg -> PackageSet pkg
insert pkg (PackageSet index) = mkPackageSet $
  Map.alter insertBucket (packageName pkg) index
  where
    insertBucket Nothing     = Just [pkg]
    insertBucket (Just pkgs) = Just (insertNoDup pkgs)
    pkgid = packageId pkg
    insertNoDup []                = [pkg]
    insertNoDup pkgs@(pkg':pkgs') = case compare pkgid (packageId pkg') of
      LT -> pkg  : pkgs
      EQ -> pkg  : pkgs'
      GT -> pkg' : insertNoDup pkgs'

-- | Internal delete helper.
--
delete :: Package pkg => PackageName -> (pkg -> Bool) -> PackageSet pkg -> PackageSet pkg
delete name p (PackageSet index) = mkPackageSet $
  Map.update filterBucket name index
  where
    filterBucket = deleteEmptyBucket
                 . filter (not . p)
    deleteEmptyBucket []        = Nothing
    deleteEmptyBucket remaining = Just remaining

-- | Removes a single package from the index.
--
deletePackageId :: Package pkg => PackageIdentifier -> PackageSet pkg -> PackageSet pkg
deletePackageId pkgid =
  delete (packageName pkgid) (\pkg -> packageId pkg == pkgid)

-- | Removes all packages with this (case-sensitive) name from the index.
--
deletePackageName :: Package pkg => PackageName -> PackageSet pkg -> PackageSet pkg
deletePackageName name =
  delete name (\pkg -> packageName pkg == name)

-- | Removes all packages satisfying this dependency from the index.
--
deleteDependency :: Package pkg => Dependency -> PackageSet pkg -> PackageSet pkg
deleteDependency (Dependency name verstionRange) =
  delete name (\pkg -> packageVersion pkg `withinRange` verstionRange)

-- | Get all the packages from the index.
--
allPackages :: Package pkg => PackageSet pkg -> [pkg]
allPackages (PackageSet m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: Package pkg => PackageSet pkg -> [[pkg]]
allPackagesByName (PackageSet m) = Map.elems m

-- | Does a lookup by package id (name & version).
--
-- Since multiple package DBs mask each other case-sensitively by package name,
-- then we get back at most one package.
--
lookupPackageId :: Package pkg => PackageSet pkg -> PackageIdentifier -> Maybe pkg
lookupPackageId index pkgid =
  case [ pkg | pkg <- lookup index (packageName pkgid)
             , packageId pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name.
--
lookupPackageName :: Package pkg => PackageSet pkg -> PackageName -> [pkg]
lookupPackageName index name =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name ]

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: Package pkg => PackageSet pkg -> Dependency -> [pkg]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg <- lookup index name
        , packageName pkg == name
        , packageVersion pkg `withinRange` versionRange ]

-- | All packages that have dependencies that are not in the index.
--
-- Returns such packages along with the dependencies that they're missing.
--
brokenPackages :: PackageFixedDeps pkg
               => PackageSet pkg
               -> [(pkg, [PackageIdentifier])]
brokenPackages index =
  [ (pkg, missing)
  | pkg  <- allPackages index
  , let missing = [ pkg' | pkg' <- depends pkg
                         , isNothing (lookupPackageId index pkg') ]
  , not (null missing) ]

-- | Tries to take the transative closure of the package dependencies.
--
-- If the transative closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if the result is @Right []@ it is because at least one of
-- the original given 'PackageIdentifier's do not occur in the index.
--
dependencyClosure :: PackageFixedDeps pkg
                  => PackageSet pkg
                  -> [PackageIdentifier]
                  -> Either (PackageSet pkg)
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

-- | Takes the transative closure of the packages reverse dependencies.
--
-- * The given 'PackageIdentifier's must be in the index.
--
reverseDependencyClosure :: PackageFixedDeps pkg
                         => PackageSet pkg
                         -> [PackageIdentifier]
                         -> [PackageIdentifier]
reverseDependencyClosure index =
    map vertexToPkgId
  . concatMap Tree.flatten
  . Graph.dfs reverseDepGraph
  . map (fromMaybe noSuchPkgId . pkgIdToVertex)

  where
    (depGraph, vertexToPkgId, pkgIdToVertex) = dependencyGraph index
    reverseDepGraph = Graph.transposeG depGraph
    noSuchPkgId = error "reverseDependencyClosure: package is not in the graph"

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
                          => PackageSet pkg
                          -> [(PackageName, [(PackageIdentifier, Version)])]
dependencyInconsistencies index =
  [ (name, inconsistencies)
  | (name, uses) <- Map.toList inverseIndex
  , let inconsistencies = duplicatesBy uses
  , not (null inconsistencies) ]

  where inverseIndex = Map.fromListWith (++)
          [ (packageName dep, [(packageId pkg, packageVersion dep)])
          | pkg <- allPackages index
          , dep <- depends pkg ]

        duplicatesBy = (\groups -> if length groups == 1
                                     then []
                                     else concat groups)
                     . groupBy (equating snd)
                     . sortBy (comparing snd)

-- | Find if there are any cycles in the dependency graph. If there are no
-- cycles the result is @[]@.
--
-- This actually computes the strongly connected components. So it gives us a
-- list of groups of packages where within each group they all depend on each
-- other, directly or indirectly.
--
dependencyCycles :: PackageFixedDeps pkg
                 => PackageSet pkg
                 -> [[pkg]]
dependencyCycles index =
  [ vs | Graph.CyclicSCC vs <- Graph.stronglyConnComp adjacencyList ]
  where
    adjacencyList = [ (pkg, packageId pkg, depends pkg)
                    | pkg <- allPackages index ]

-- | Builds a graph of the package dependencies.
--
-- Dependencies on other packages that are in the index are discarded.
-- You can check if there are any such dependencies with 'brokenPackages'.
--
dependencyGraph :: PackageFixedDeps pkg
                => PackageSet pkg
                -> (Graph.Graph,
                    Graph.Vertex -> PackageIdentifier,
                    PackageIdentifier -> Maybe Graph.Vertex)
dependencyGraph index = (graph, vertexToPkgId, pkgIdToVertex)
  where
    graph = Array.listArray bounds
              [ [ v | Just v <- map pkgIdToVertex (depends pkg) ]
              | pkg <- pkgs ]
    vertexToPkgId vertex = pkgIdTable ! vertex
    pkgIdToVertex = binarySearch 0 topBound

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

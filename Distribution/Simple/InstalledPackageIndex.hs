-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.InstalledPackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  Duncan Coutts <duncan@haskell.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- The index of 'InstalledPackageInfo'.
-----------------------------------------------------------------------------
module Distribution.Simple.InstalledPackageIndex (
  -- * Local installed index data type
  InstalledPackageIndex,

  -- * Creating the index
  fromList,

  -- * Merging indexes
  merge,

  -- * Queries

  -- ** Precise lookups
  lookupPackageId,
  lookupDependency,

  -- ** Case-insensitive searches
  searchByName,
  SearchResult(..),
  searchByNameSubstring,

  -- ** Bulk queries
  allPackages,
  allPackagesByName,
  ) where

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nubBy, group, sort, groupBy, sortBy, find)
import Data.Monoid (Monoid(..))

import Distribution.Package (PackageIdentifier(..))
import Distribution.InstalledPackageInfo
         (InstalledPackageInfo, InstalledPackageInfo_(..))
import Distribution.Version (Dependency(Dependency), withinRange)
import Distribution.Simple.Utils (lowercase, equating, comparing, isInfixOf)

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched effeciently by package name and version.
--
data InstalledPackageIndex = InstalledPackageIndex
  -- | This index maps lower case package names to all the
  -- 'InstalledPackageInfo' records matching that package name
  -- case-insensitively. It includes all versions.
  --
  -- This allows us to do case sensitive or insensitive lookups, and to find
  -- all versions satisfying a dependency, all by varying how we filter. So
  -- most queries will do a map lookup followed by a linear scan of the bucket.
  --
  (Map String [InstalledPackageInfo])

  deriving (Show, Read)

instance Monoid InstalledPackageIndex where
  mempty  = InstalledPackageIndex (Map.empty)
  mappend = merge
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

invariant :: InstalledPackageIndex -> Bool
invariant (InstalledPackageIndex m) = all (uncurry goodBucket) (Map.toList m)
  where goodBucket name pkgs =
             lowercase name == name
          && not (null pkgs)
          && all ((lowercase name==) . lowercase . pkgName . package) pkgs
--          && all (\pkg -> pkgInfoId pkg
--                       == (package . packageDescription . pkgDesc) pkg) pkgs
          && distinct (map package pkgs)

        distinct = all ((==1). length) . group . sort

internalError :: String -> a
internalError name = error ("InstalledPackageIndex." ++ name ++ ": internal error")

-- | When building or merging we have to eliminate duplicates of the exact
-- same package name and version (case-sensitively) to preserve the invariant.
--
stripDups :: [InstalledPackageInfo] -> [InstalledPackageInfo]
stripDups = nubBy (equating package)

-- | Lookup a name in the index to get all packages that match that name
-- case-insensitively.
--
lookup :: InstalledPackageIndex -> String -> [InstalledPackageInfo]
lookup index@(InstalledPackageIndex m) name =
  assert (invariant index) $
  case Map.lookup (lowercase name) m of
    Nothing   -> []
    Just pkgs -> pkgs

-- | Build an index out of a bunch of 'InstalledPackageInfo's.
--
-- If there are duplicates, earlier ones mask later one.
--
fromList :: [InstalledPackageInfo] -> InstalledPackageIndex
fromList pkgs =
  let index = (InstalledPackageIndex . Map.map stripDups . Map.fromListWith (++))
                [ let key = (lowercase . pkgName . package) pkg
                   in (key, [pkg])
                | pkg <- pkgs ]
   in assert (invariant index) index

-- | Merge two indexes.
--
-- Packages from the first mask packages of the same exact name
-- (case-sensitively) from the second.
--
merge :: InstalledPackageIndex -> InstalledPackageIndex -> InstalledPackageIndex
merge i1@(InstalledPackageIndex m1) i2@(InstalledPackageIndex m2) =
  assert (invariant i1 && invariant i2) $
  let index = InstalledPackageIndex (Map.unionWith mergeBuckets m1 m2)
   in assert (invariant index) index

  where mergeBuckets pkgs1 pkgs2 = stripDups (pkgs1 ++ pkgs2)

-- | Get all the packages from the index.
--
allPackages :: InstalledPackageIndex -> [InstalledPackageInfo]
allPackages (InstalledPackageIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: InstalledPackageIndex -> [[InstalledPackageInfo]]
allPackagesByName (InstalledPackageIndex m) = concatMap groupByName (Map.elems m)
  where groupByName :: [InstalledPackageInfo] -> [[InstalledPackageInfo]]
        groupByName = groupBy (equating (pkgName . package))
                    . sortBy (comparing (pkgName . package))

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
searchByName :: InstalledPackageIndex -> String -> SearchResult [InstalledPackageInfo]
searchByName index name =
  case groupBy (equating  (pkgName . package))
     . sortBy  (comparing (pkgName . package))
     $ lookup index name of
    []     -> None
    [pkgs] -> Unambiguous pkgs
    pkgss  -> case find ((name==) . pkgName . package . head) pkgss of
                Just pkgs -> Unambiguous pkgs
                Nothing   -> Ambiguous   pkgss

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: InstalledPackageIndex -> String -> [InstalledPackageInfo]
searchByNameSubstring (InstalledPackageIndex m) searchterm =
  [ pkg
  | (name, pkgs) <- Map.toList m
  , searchterm' `isInfixOf` name
  , pkg <- pkgs ]
  where searchterm' = lowercase searchterm

-- | Does a lookup by package id (name & version).
--
-- Since multiple package DBs mask each other case-sensitively by package name,
-- then we get back at most one package.
--
lookupPackageId :: InstalledPackageIndex -> PackageIdentifier -> Maybe InstalledPackageInfo
lookupPackageId index pkgid =
  case [ pkg | pkg <- lookup index (pkgName pkgid)
             , package pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: InstalledPackageIndex -> Dependency -> [InstalledPackageInfo]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg@InstalledPackageInfo { package = pkgid } <- lookup index name
        , pkgName pkgid == name
        , pkgVersion pkgid `withinRange` versionRange ]

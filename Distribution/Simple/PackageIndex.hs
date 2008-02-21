-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PackageIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  Duncan Coutts <duncan@haskell.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- An index of packages.
-----------------------------------------------------------------------------
module Distribution.Simple.PackageIndex (
  -- * Package classes
  Package(..),
  PackageFixedDeps(..),

  -- * Package index data type
  PackageIndex,

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

  -- ** Special queries
  brokenPackages,
  dependencyClosure,
  dependencyInconsistencies
  ) where

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nubBy, group, sort, groupBy, sortBy, find)
import Data.Monoid (Monoid(..))
import Data.Maybe (isNothing)

import Distribution.Package (PackageIdentifier(..))
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo_ )
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
         ( InstalledPackageInfo_(..) )
import Distribution.PackageDescription
         ( PackageDescription, GenericPackageDescription )
import qualified Distribution.PackageDescription as PackageDescription
         ( PackageDescription(..), GenericPackageDescription(packageDescription) )
import Distribution.Version (Version, Dependency(Dependency), withinRange)
import Distribution.Simple.Utils (lowercase, equating, comparing, isInfixOf)

-- | Class of things that can be identified by a 'PackageIdentifier'
--
-- Types in this class are all notions of a package. This allows us to have
-- different types for the different phases that packages go though, from
-- simple name\/id, package description, configured or installed packages.
--
class Package pkg where
  packageId :: pkg -> PackageIdentifier

instance Package PackageIdentifier where
  packageId = id
instance Package (InstalledPackageInfo_ str) where
  packageId = InstalledPackageInfo.package
instance Package PackageDescription where
  packageId = PackageDescription.package
instance Package GenericPackageDescription where
  packageId = packageId . PackageDescription.packageDescription

-- | Subclass of packages that have specific versioned dependencies.
--
-- So for example a not-yet-configured package has dependencies on version
-- ranges, not specific versions. A configured or an already installed package
-- depends on exact versions. Some operations or data structures (like
--  dependency graphs) only make sense on this subclass of package types.
--
class Package pkg => PackageFixedDeps pkg where
  depends :: pkg -> [PackageIdentifier]

instance PackageFixedDeps (InstalledPackageInfo_ str) where
  depends = InstalledPackageInfo.depends

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched effeciently by package name and version.
--
data Package pkg => PackageIndex pkg = PackageIndex
  -- This index maps lower case package names to all the
  -- 'InstalledPackageInfo' records matching that package name
  -- case-insensitively. It includes all versions.
  --
  -- This allows us to do case sensitive or insensitive lookups, and to find
  -- all versions satisfying a dependency, all by varying how we filter. So
  -- most queries will do a map lookup followed by a linear scan of the bucket.
  --
  (Map String [pkg])

  deriving (Show, Read)

instance Package pkg => Monoid (PackageIndex pkg) where
  mempty  = PackageIndex (Map.empty)
  mappend = merge
  --save one mappend with empty in the common case:
  mconcat [] = mempty
  mconcat xs = foldr1 mappend xs

invariant :: Package pkg => PackageIndex pkg -> Bool
invariant (PackageIndex m) = all (uncurry goodBucket) (Map.toList m)
  where goodBucket name pkgs =
             lowercase name == name
          && not (null pkgs)
          && all ((lowercase name==) . lowercase . pkgName . packageId) pkgs
--          && all (\pkg -> pkgInfoId pkg
--                       == (packageId . packageDescription . pkgDesc) pkg) pkgs
          && distinct (map packageId pkgs)

        distinct = all ((==1). length) . group . sort

internalError :: String -> a
internalError name = error ("PackageIndex." ++ name ++ ": internal error")

-- | When building or merging we have to eliminate duplicates of the exact
-- same package name and version (case-sensitively) to preserve the invariant.
--
stripDups :: Package pkg => [pkg] -> [pkg]
stripDups = nubBy (equating packageId)

-- | Lookup a name in the index to get all packages that match that name
-- case-insensitively.
--
lookup :: Package pkg => PackageIndex pkg -> String -> [pkg]
lookup index@(PackageIndex m) name =
  assert (invariant index) $
  case Map.lookup (lowercase name) m of
    Nothing   -> []
    Just pkgs -> pkgs

-- | Build an index out of a bunch of 'Package's.
--
-- If there are duplicates, earlier ones mask later one.
--
fromList :: Package pkg => [pkg] -> PackageIndex pkg
fromList pkgs =
  let index = (PackageIndex . Map.map stripDups . Map.fromListWith (++))
                [ let key = (lowercase . pkgName . packageId) pkg
                   in (key, [pkg])
                | pkg <- pkgs ]
   in assert (invariant index) index

-- | Merge two indexes.
--
-- Packages from the first mask packages of the same exact name
-- (case-sensitively) from the second.
--
merge :: Package pkg => PackageIndex pkg -> PackageIndex pkg -> PackageIndex pkg
merge i1@(PackageIndex m1) i2@(PackageIndex m2) =
  assert (invariant i1 && invariant i2) $
  let index = PackageIndex (Map.unionWith mergeBuckets m1 m2)
   in assert (invariant index) index

  where mergeBuckets pkgs1 pkgs2 = stripDups (pkgs1 ++ pkgs2)

-- | Get all the packages from the index.
--
allPackages :: Package pkg => PackageIndex pkg -> [pkg]
allPackages (PackageIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: Package pkg => PackageIndex pkg -> [[pkg]]
allPackagesByName (PackageIndex m) = concatMap groupByName (Map.elems m)
  where groupByName :: Package pkg => [pkg] -> [[pkg]]
        groupByName = groupBy (equating (pkgName . packageId))
                    . sortBy (comparing (pkgName . packageId))

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
searchByName index name =
  case groupBy (equating  (pkgName . packageId))
     . sortBy  (comparing (pkgName . packageId))
     $ lookup index name of
    []     -> None
    [pkgs] -> Unambiguous pkgs
    pkgss  -> case find ((name==) . pkgName . packageId . head) pkgss of
                Just pkgs -> Unambiguous pkgs
                Nothing   -> Ambiguous   pkgss

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
searchByNameSubstring :: Package pkg => PackageIndex pkg -> String -> [pkg]
searchByNameSubstring (PackageIndex m) searchterm =
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
lookupPackageId :: Package pkg => PackageIndex pkg -> PackageIdentifier -> Maybe pkg
lookupPackageId index pkgid =
  case [ pkg | pkg <- lookup index (pkgName pkgid)
             , packageId pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: Package pkg => PackageIndex pkg -> Dependency -> [pkg]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg <- lookup index name
        , let pkgid = packageId pkg
        , pkgName pkgid == name
        , pkgVersion pkgid `withinRange` versionRange ]

-- | All packages that have depends that are not in the index.
--
-- Returns such packages along with the depends that they're missing.
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

-- | Tries to take the transative closure of the package dependencies.
--
-- If the transative closure is complete then it returns that subset of the
-- index. Otherwise it returns the broken packages as in 'brokenPackages'.
--
-- * Note that if any of the result is @Right []@ it is because at least one of
-- the original given 'PackageIdentifier's do not occur in the index.
--
dependencyClosure :: PackageFixedDeps pkg
                  => PackageIndex pkg
                  -> [PackageIdentifier]
                  -> Either (PackageIndex pkg)
                            [(pkg, [PackageIdentifier])]
dependencyClosure index pkgids0 = case closure [] [] pkgids0 of
  (completed, []) -> Left  $ fromList completed
  (completed, _)  -> Right $ brokenPackages (fromList completed)

  where
    closure completed failed [] = (completed, failed)
    closure completed failed (pkgid:pkgids) = case lookupPackageId index pkgid of
      Nothing  -> closure completed (pkgid:failed) pkgids
               -- TODO: use more effecient test here:
      Just pkg | packageId pkg `elem` map packageId completed
               -> closure      completed   failed  pkgids
               | otherwise
               -> closure (pkg:completed)  failed  pkgids'
               where pkgids' = depends pkg ++ pkgids

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
                          -> pkg
                          -> [(String, [(PackageIdentifier, Version)])]
dependencyInconsistencies index topPkg =
  [ (name, inconsistencies)
  | (name, uses) <- Map.toList inverseIndex
  , let inconsistencies = duplicatesBy uses
  , not (null inconsistencies) ]

  where inverseIndex = Map.fromListWith (++)
          [ (pkgName dep, [(packageId pkg, pkgVersion dep)])
          | pkg <- topPkg : allPackages index
          , dep <- depends pkg ]

        duplicatesBy = (\groups -> if length groups == 1
                                     then []
                                     else concat groups)
                     . groupBy (equating snd)
                     . sortBy (comparing snd)

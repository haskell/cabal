-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.LocalIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The installed package index.
-----------------------------------------------------------------------------
module Hackage.LocalIndex (
  -- * Local installed index data type
  LocalIndex,

  -- * Getting the index from the compiler
  Hackage.LocalIndex.read,

  -- * Merging indexes
  merge,

  -- * Queries
  allPackages,
  allPackageGroups,
  lookupPackageName,
  SearchResult(..),
  lookupPackageNameSubstring,
  lookupPackageIdentifier,
  lookupDependency,
  ) where

import Hackage.Utils (lowercase, equating, comparing, isInfixOf)

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nub, group, sort, groupBy, sortBy, find)
import Data.Monoid (Monoid(..))

import Distribution.Package (PackageIdentifier(..))
import Distribution.Version (Dependency(Dependency), withinRange)
import Distribution.Verbosity (Verbosity)

import Distribution.Simple.Compiler (Compiler, PackageDB(..))
import Distribution.Simple.Configure (getInstalledPackages)
import Distribution.Simple.Program (ProgramConfiguration)

-- | The collection of information about packages from one or more 'PackageDB's.
--
-- It can be searched effeciently by package name and version.
--
data LocalIndex = LocalIndex
  -- | This index maps lower case package names to all the 'PackageIdentifier'
  -- records matching that package name case-insensitively. It includes all
  -- versions.
  -- 
  -- This allows us to do case sensitive or insensitive lookups, and to find
  -- all versions satisfying a dependency, all by varying how we filter. So
  --most queries will do a map lookup followed by a linear scan of the bucket.
  --
  (Map String [PackageIdentifier])

--FIXME: we should be keeping full 'InstalledPackageInfo's rather than just
-- 'PackageIdentifier'. That involves a ghc-pkg dump feature and for nhc98 and
-- hugs it'll probably involve reading a bunch of 'InstalledPackageInfo' files.

instance Monoid LocalIndex where
  mempty  = LocalIndex (Map.empty)
  mappend = merge

invariant :: LocalIndex -> Bool
invariant (LocalIndex m) = all (uncurry goodBucket) (Map.toList m)
  where goodBucket name pkgs =
             lowercase name == name
          && not (null pkgs)
          && all ((lowercase name==) . lowercase . pkgName) pkgs
--          && all (\pkg -> pkgInfoId pkg
--                       == (package . packageDescription . pkgDesc) pkg) pkgs
          && distinct pkgs
        
        distinct = all ((==1). length) . group . sort

internalError :: String -> a
internalError name = error ("LocalIndex." ++ name ++ ": internal error")

-- | When building or merging we have to eliminate duplicates of the exact
-- same package name and version (case-sensitively) to preserve the invariant.
--
stripDups :: [PackageIdentifier] -> [PackageIdentifier]
stripDups = nub

-- | Lookup a name in the index to get all packages that match that name
-- case-insensitively.
--
lookup :: LocalIndex -> String -> [PackageIdentifier]
lookup index@(LocalIndex m) name =
  assert (invariant index) $
  case Map.lookup (lowercase name) m of
    Nothing   -> []
    Just pkgs -> pkgs

-- | Build an index out of a bunch of 'PackageIdentifier's.
--
-- If there are duplicates, earlier ones mask later one.
--
build :: [PackageIdentifier] -> LocalIndex
build pkgs = 
  let index = (LocalIndex . Map.map stripDups . Map.fromListWith (++))
                [ let key = (lowercase . pkgName) pkg
                   in (key, [pkg])
                | pkg <- pkgs ]
   in assert (invariant index) index

-- | Get all the packages from the index.
--
allPackages :: LocalIndex -> [PackageIdentifier]
allPackages (LocalIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-insensitively.
--
allPackageGroups :: LocalIndex -> [[PackageIdentifier]]
allPackageGroups (LocalIndex m) = Map.elems m

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
lookupPackageName :: LocalIndex -> String -> SearchResult [PackageIdentifier]
lookupPackageName index name =
  case groupBy (equating  (pkgName))
     . sortBy  (comparing (pkgName))
     $ lookup index name of
    []     -> None
    [pkgs] -> Unambiguous pkgs
    pkgss  -> case find ((name==) . pkgName . head) pkgss of
                Just pkgs -> Unambiguous pkgs
                Nothing   -> Ambiguous   pkgss

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
lookupPackageNameSubstring :: LocalIndex -> String -> [PackageIdentifier]
lookupPackageNameSubstring (LocalIndex m) searchterm =
  [ pkg
  | (name, pkgs) <- Map.toList m
  , searchterm' `isInfixOf` name
  , pkg <- pkgs ]
  where searchterm' = lowercase searchterm

-- | Does a case-sensitive search by package name, and also version.
--
-- Since multiple repos mask each other case-sensitively by package name, then
-- we get back at most one package.
--
lookupPackageIdentifier :: LocalIndex -> PackageIdentifier -> Maybe PackageIdentifier
lookupPackageIdentifier index (PackageIdentifier name version) =
  case [ pkgid | pkgid <- lookup index name
             , pkgName pkgid == name
             , pkgVersion pkgid == version ] of
    []      -> Nothing
    [pkgid] -> Just pkgid
    _       -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: LocalIndex -> Dependency -> [PackageIdentifier]
lookupDependency index (Dependency name versionRange) =
  [ pkgid | pkgid <- lookup index name
          , pkgName pkgid == name
          , pkgVersion pkgid `withinRange` versionRange ]

-- | Merge two indexes.
--
-- Packages from the first mask packages of the same exact name
-- (case-sensitively) from the second.
--
merge :: LocalIndex -> LocalIndex -> LocalIndex
merge i1@(LocalIndex m1) i2@(LocalIndex m2) =
  assert (invariant i1 && invariant i2) $
  let index = LocalIndex (Map.unionWith mergeBuckets m1 m2)
   in assert (invariant index) index

  where mergeBuckets pkgs1 pkgs2 = stripDups (pkgs1 ++ pkgs2)


-- | Read an installed package index by asking the compiler.
--
read :: Verbosity -> Compiler -> ProgramConfiguration -> PackageDB -> IO LocalIndex
read verbosity comp conf packageDB = do
  Just pkgs <- getInstalledPackages verbosity comp packageDB conf
  return (build pkgs)

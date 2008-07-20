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
  ) where

import Prelude hiding (lookup)
import Control.Exception (assert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (groupBy, sortBy, find)
import Data.Monoid (Monoid(..))

import Distribution.Package
         ( PackageName(PackageName), PackageIdentifier, Package(..)
         , packageName, packageVersion, Dependency(Dependency) )
import Distribution.Version
         ( withinRange )
import Distribution.Simple.Utils (lowercase, equating, comparing, isInfixOf)

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
import Text.Read
import qualified Text.Read.Lex as L
#endif

newtype Key = Key String deriving (Eq, Ord, Read, Show)

searchKey :: String -> Key
searchKey = Key . lowercase

packageNameKey :: PackageName -> Key
packageNameKey (PackageName n) = searchKey n

packageKey :: Package pkg => pkg -> Key
packageKey = packageNameKey . packageName

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
  (Map Key [pkg])

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
    goodBucket _   [] = False
    goodBucket key (pkg0:pkgs0) = check (packageId pkg0) pkgs0
      where
        check pkgid []          = packageKey pkgid == key
        check pkgid (pkg':pkgs) = packageKey pkgid == key
                               && pkgid < pkgid'
                               && check pkgid' pkgs
          where pkgid' = packageId pkg'

mkPackageIndex :: Package pkg => Map Key [pkg] -> PackageIndex pkg
mkPackageIndex index = assert (invariant (PackageIndex index))
                                         (PackageIndex index)

internalError :: String -> a
internalError name = error ("PackageIndex." ++ name ++ ": internal error")

-- | Lookup a name in the index to get all packages that match that name
-- case-insensitively.
--
lookup :: Package pkg => PackageIndex pkg -> Key -> [pkg]
lookup (PackageIndex m) key =
  case Map.lookup key m of
    Nothing   -> []
    Just pkgs -> pkgs

-- | Build an index out of a bunch of 'Package's.
--
-- If there are duplicates, later ones mask earlier ones.
--
fromList :: Package pkg => [pkg] -> PackageIndex pkg
fromList pkgs = mkPackageIndex
              . Map.map fixBucket
              . Map.fromListWith (++)
              $ [ let key = packageKey pkg
                   in (key, [pkg])
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
  let key = packageKey pkg
   in Map.insertWith (\_ -> insertNoDup) key [pkg] index
  where
    pkgid = packageId pkg
    insertNoDup []                = [pkg]
    insertNoDup pkgs@(pkg':pkgs') = case compare pkgid (packageId pkg') of
      LT -> pkg  : pkgs
      EQ -> pkg  : pkgs'
      GT -> pkg' : insertNoDup pkgs'

-- | Internal delete helper.
--
delete :: Package pkg
       => PackageName -> (pkg -> Bool)
       -> PackageIndex pkg -> PackageIndex pkg
delete name p (PackageIndex index) = mkPackageIndex $
  let key = packageNameKey name
   in Map.update filterBucket key index
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

-- | Get all the packages from the index.
--
allPackages :: Package pkg => PackageIndex pkg -> [pkg]
allPackages (PackageIndex m) = concat (Map.elems m)

-- | Get all the packages from the index.
--
-- They are grouped by package name, case-sensitively.
--
allPackagesByName :: Package pkg => PackageIndex pkg -> [[pkg]]
allPackagesByName (PackageIndex m) =
  concatMap (groupBy (equating packageName)) (Map.elems m)

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
  case groupBy (equating packageName) (lookup index (searchKey name)) of
    []     -> None
    [pkgs] -> Unambiguous pkgs
    pkgss  -> case find ((PackageName name==) . packageName . head) pkgss of
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
  | (Key name, pkgs) <- Map.toList m
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
  case [ pkg | pkg <- lookup index (packageKey pkgid)
             , packageId pkg == pkgid ] of
    []    -> Nothing
    [pkg] -> Just pkg
    _     -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name.
--
lookupPackageName :: Package pkg => PackageIndex pkg -> PackageName -> [pkg]
lookupPackageName index name =
  [ pkg | pkg <- lookup index (packageNameKey name)
        , packageName pkg == name ]

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: Package pkg => PackageIndex pkg -> Dependency -> [pkg]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg <- lookup index (packageNameKey name)
        , packageName pkg == name
        , packageVersion pkg `withinRange` versionRange ]

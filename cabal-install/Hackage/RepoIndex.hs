-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.RepoIndex
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007,
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- The downloaded package index of a remote repo.
-----------------------------------------------------------------------------
module Hackage.RepoIndex (
  -- * Repository index data type
  RepoIndex,

  -- * Reading from file or memory
  Hackage.RepoIndex.read,
  parse,

  -- * Merging indexes
  merge,

  -- * Queries
  allPackages,
  lookupPackageName,
  SearchResult(..),
  lookupPackageNameSubstring,
  lookupPackageIdentifier,
  lookupDependency,
  ) where

import Hackage.Types
import Hackage.Tar
import Hackage.Utils (lowercase, equating, comparing, isInfixOf)

import Prelude hiding (catch, lookup)
import Control.Exception (catch, Exception(IOException), assert)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (nubBy, group, sort, groupBy, sortBy, find)
import Data.Monoid (Monoid(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.IO.Error (isDoesNotExistError)

import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
--         PackageDescription(package),
--          GenericPackageDescription(packageDescription))
import Distribution.Package (PackageIdentifier(..))
import Distribution.Version (readVersion, Dependency(Dependency), withinRange)
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (warn)

-- | The collection of information about packages from one or more 'Repo's.
--
-- It can be searched effeciently by package name and version.
--
-- This encodes how we deal with finding package when we have multiple
-- repositories. There are some interesting issues with case-sensitivity
-- and when multiple repos supply the \"same\" package. The behaviour we want
-- is that in general repos mask one another where they overlap. So the index
-- will give at most one package for each case-sensitive name-version lookup,
-- that is we take it from the first repo we find it in. Dependencies have to
-- be satisfied in a case-sensitive manner. On the other hand it is very
-- convenient for users to be able to specify packages in a case insensitive
-- manner. So lookup by package name is case insensitive but fails if the
-- search is ambigious.
--
-- We may want to extend this index in future with a full text search index.
-- If it is too slow then we could use a trie on the package name and version.
-- Furthermore we may want to cache this index on disk and re-generate it when
-- we download the tar file from the remote repo.
--
data RepoIndex = RepoIndex
  -- | This index maps lower case package names to all the PkgInfo records
  -- matching that package name case-insensitively. It includes all versions
  -- and may include the same version from different repos, with earlier ones
  -- masking later ones.
  -- 
  -- This allows us to do case sensitive or insensitive lookups, to pick the
  -- first repo or all, and to find all versions satisfying a dependency,
  -- all by varying how we filter the [PkgInfo]. So most queries will do a
  -- map lookup followed by a linear scan of the [PkgInfo] bucket.
  --
  (Map String [PkgInfo])

instance Monoid RepoIndex where
  mempty  = RepoIndex (Map.empty)
  mappend = merge

invariant :: RepoIndex -> Bool
invariant (RepoIndex m) = all (uncurry goodBucket) (Map.toList m)
  where goodBucket name pkgs =
             lowercase name == name
          && not (null pkgs)
          && all ((lowercase name==) . lowercase . pkgName . pkgInfoId) pkgs
--          && all (\pkg -> pkgInfoId pkg
--                       == (package . packageDescription . pkgDesc) pkg) pkgs
          && distinct (map pkgInfoId pkgs)
        
        distinct = all ((==1). length) . group . sort

internalError :: String -> a
internalError name = error ("RepoIndex." ++ name ++ ": internal error")

-- | When building or merging we have to eliminate duplicates of the exact
-- same package name and version (case-sensitively) to preserve the invariant.
--
stripDups :: [PkgInfo] -> [PkgInfo]
stripDups = nubBy (equating pkgInfoId)

-- | Lookup a name in the index to get all packages that match that name
-- case-insensitively.
--
lookup :: RepoIndex -> String -> [PkgInfo]
lookup index@(RepoIndex m) name =
  assert (invariant index) $
  case Map.lookup (lowercase name) m of
    Nothing   -> []
    Just pkgs -> pkgs

-- | Build an index out of a bunch of 'PkgInfo's.
--
-- If there are duplicates, earlier ones mask later one.
--
build :: [PkgInfo] -> RepoIndex
build pkgs = 
  let index = (RepoIndex . Map.map stripDups . Map.fromListWith (++))
                [ let key = (lowercase . pkgName . pkgInfoId) pkg
                   in (key, [pkg])
                | pkg <- pkgs ]
   in assert (invariant index) index

-- | Get all the packages from the index.
--
allPackages :: RepoIndex -> [PkgInfo]
allPackages (RepoIndex m) = concat (Map.elems m)

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
lookupPackageName :: RepoIndex -> String -> SearchResult [PkgInfo]
lookupPackageName index name =
  case groupBy (equating  (pkgName . pkgInfoId))
     . sortBy  (comparing (pkgName . pkgInfoId))
     $ lookup index name of
    []     -> None
    [pkgs] -> Unambiguous pkgs
    pkgss  -> case find ((name==) . pkgName . pkgInfoId . head) pkgss of
                Just pkgs -> Unambiguous pkgs
                Nothing   -> Ambiguous   pkgss

data SearchResult a = None | Unambiguous a | Ambiguous [a]

-- | Does a case-insensitive substring search by package name.
--
-- That is, all packages that contain the given string in their name.
--
lookupPackageNameSubstring :: RepoIndex -> String -> [PkgInfo]
lookupPackageNameSubstring (RepoIndex m) searchterm =
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
lookupPackageIdentifier :: RepoIndex -> PackageIdentifier -> Maybe PkgInfo
lookupPackageIdentifier index (PackageIdentifier name version) =
  case [ pkg | pkg@PkgInfo { pkgInfoId = pkgid } <- lookup index name
             , pkgName pkgid == name
             , pkgVersion pkgid == version ] of
    []      -> Nothing
    [pkg]   -> Just pkg
    _       -> internalError "lookupPackageIdentifier"

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
lookupDependency :: RepoIndex -> Dependency -> [PkgInfo]
lookupDependency index (Dependency name versionRange) =
  [ pkg | pkg@PkgInfo { pkgInfoId = pkgid } <- lookup index name
        , pkgName pkgid == name
        , pkgVersion pkgid `withinRange` versionRange ]

-- | Merge two indexes.
--
-- Packages from the first mask packages of the same exact name
-- (case-sensitively) from the second.
--
merge :: RepoIndex -> RepoIndex -> RepoIndex
merge i1@(RepoIndex m1) i2@(RepoIndex m2) =
  assert (invariant i1 && invariant i2) $
  let index = RepoIndex (Map.unionWith mergeBuckets m1 m2)
   in assert (invariant index) index

  where mergeBuckets pkgs1 pkgs2 = stripDups (pkgs1 ++ pkgs2)


-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
read :: Verbosity -> Repo -> IO RepoIndex
read verbosity repo =
    do let indexFile = repoCacheDir repo </> "00-index.tar"
       fmap (parse repo) (BS.readFile indexFile)
          `catch` (\e -> do case e of
                              IOException ioe | isDoesNotExistError ioe ->
                                warn verbosity "The package list does not exist. Run 'cabal update' to download it."
                              _ -> warn verbosity (show e)
                            return (build []))

-- | Parse a repository index file from a 'ByteString'.
--
-- All the 'PkgInfo's are marked as having come from the given 'Repo'.
--
parse :: Repo -> ByteString -> RepoIndex
parse repo s = build $
    do (hdr, content) <- readTarArchive s
       if takeExtension (tarFileName hdr) == ".cabal"
         then case splitDirectories (normalise (tarFileName hdr)) of
                [pkgname,vers,_] ->
                  let descr = case parsePackageDescription (BS.Char8.unpack content) of
                        ParseOk _ d -> d
                        _           -> error $ "Couldn't read cabal file "
                                            ++ show (tarFileName hdr)
                   in case readVersion vers of
                        Just ver ->
                         return $ PkgInfo {
                                 pkgInfoId = PackageIdentifier pkgname ver,
                                 pkgRepo = repo,
                                 pkgDesc = descr
                               }
                        _ -> []
                _ -> []
         else []

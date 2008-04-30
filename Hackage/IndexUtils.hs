-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Hackage.IndexUtils (
  readRepoIndex,
  disambiguatePackageName,
  disambiguateDependencies
  ) where

import Hackage.Tar
import Hackage.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo(..) )

import Distribution.Package (PackageIdentifier(..), Package(..), Dependency(Dependency))
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult(..))
import Distribution.Text
         ( simpleParse )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (die, warn, intercalate, fromUTF8)

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.IO.Error (isDoesNotExistError)

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
readRepoIndex :: Verbosity -> Repo -> IO (PackageIndex AvailablePackage)
readRepoIndex verbosity repo =
  let indexFile = repoCacheDir repo </> "00-index.tar"
   in fmap parseRepoIndex (BS.readFile indexFile)
          `catch` (\e -> do case e of
                              IOException ioe | isDoesNotExistError ioe ->
                                warn verbosity "The package list does not exist. Run 'cabal update' to download it."
                              _ -> warn verbosity (show e)
                            return (PackageIndex.fromList []))

  where
    -- | Parse a repository index file from a 'ByteString'.
    --
    -- All the 'AvailablePackage's are marked as having come from the given 'Repo'.
    --
    parseRepoIndex :: ByteString -> PackageIndex AvailablePackage
    parseRepoIndex s = PackageIndex.fromList $ do
      (hdr, content) <- readTarArchive s
      if takeExtension (tarFileName hdr) == ".cabal"
        then case splitDirectories (normalise (tarFileName hdr)) of
               [pkgname,vers,_] ->
                 let parsed = parsePackageDescription
                                (fromUTF8 . BS.Char8.unpack $ content)
                     descr  = case parsed of
                       ParseOk _ d -> d
                       _           -> error $ "Couldn't read cabal file "
                                           ++ show (tarFileName hdr)
                  in case simpleParse vers of
                       Just ver -> return AvailablePackage {
                           packageInfoId = PackageIdentifier pkgname ver,
                           packageDescription = descr,
                           packageSource = RepoTarballPackage repo
                         }
                       _ -> []
               _ -> []
        else []

-- | Disambiguate a set of packages using 'disambiguatePackage' and report any
-- ambiguities to the user.
--
disambiguateDependencies :: PackageIndex AvailablePackage
                         -> [UnresolvedDependency]
                         -> IO [UnresolvedDependency]
disambiguateDependencies index deps = do
  let names = [ (name, disambiguatePackageName index name)
              | UnresolvedDependency (Dependency name _) _ <- deps ]
   in case [ (name, matches) | (name, Right matches) <- names ] of
        []        -> return
          [ UnresolvedDependency (Dependency name vrange) flags
          | (UnresolvedDependency (Dependency _ vrange) flags,
             (_, Left name)) <- zip deps names ]
        ambigious -> die $ unlines
          [ if null matches
              then "There is no package named " ++ name
              else "The package name " ++ name ++ "is ambigious. "
                ++ "It could be: " ++ intercalate ", " matches
          | (name, matches) <- ambigious ]

-- | Given an index of known packages and a package name, figure out which one it
-- might be referring to. If there is an exact case-sensitive match then that's
-- ok. If it matches just one package case-insensitively then that's also ok.
-- The only problem is if it matches multiple packages case-insensitively, in
-- that case it is ambigious.
--
disambiguatePackageName :: PackageIndex AvailablePackage
                        -> String
                        -> Either String [String]
disambiguatePackageName index name =
    case PackageIndex.searchByName index name of
      PackageIndex.None              -> Right []
      PackageIndex.Unambiguous pkgs  -> Left (pkgName (packageId (head pkgs)))
      PackageIndex.Ambiguous   pkgss -> Right [ pkgName (packageId pkg)
                                           | (pkg:_) <- pkgss ]

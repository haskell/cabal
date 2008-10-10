-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Distribution.Client.IndexUtils (
  getAvailablePackages,

  readPackageIndexFile,
  parseRepoIndex,

  disambiguatePackageName,
  disambiguateDependencies
  ) where

import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types
         ( UnresolvedDependency(..), AvailablePackage(..)
         , AvailablePackageSource(..), Repo(..), RemoteRepo(..)
         , AvailablePackageDb(..) )

import Distribution.Package
         ( PackageId, PackageIdentifier(..), PackageName(..), Package(..)
         , Dependency(Dependency) )
import Distribution.Simple.PackageIndex (PackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.PackageDescription
         ( GenericPackageDescription )
import Distribution.PackageDescription.Parse
         ( parsePackageDescription )
import Distribution.ParseUtils
         ( ParseResult(..) )
import Distribution.Version
         ( VersionRange(IntersectVersionRanges) )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (die, warn, info, intercalate, fromUTF8)

import Data.Maybe  (catMaybes, fromMaybe)
import Data.List   (isPrefixOf)
import Data.Monoid (Monoid(..))
import qualified Data.Map as Map
import Control.Monad (MonadPlus(mplus))
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import qualified Codec.Compression.GZip as GZip (decompress)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO.Error (isDoesNotExistError)

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'AvailablePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
getAvailablePackages :: Verbosity -> [Repo] -> IO AvailablePackageDb
getAvailablePackages verbosity repos = do
  info verbosity "Reading available packages..."
  pkgss <- mapM (readRepoIndex verbosity) repos
  let (pkgs, prefs) = mconcat pkgss
      prefs' = Map.fromListWith IntersectVersionRanges
                 [ (name, range) | Dependency name range <- prefs ]
  evaluate pkgs
  evaluate prefs'
  return AvailablePackageDb {
    packageIndex       = pkgs,
    packagePreferences = prefs'
  }

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'AvailablePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
readRepoIndex :: Verbosity -> Repo
              -> IO (PackageIndex AvailablePackage, [Dependency])
readRepoIndex verbosity repo = handleNotFound $ do
  let indexFile = repoLocalDir repo </> "00-index.tar"
  (pkgs, prefs) <- either fail return
                 . foldlTarball extract ([], [])
               =<< BS.readFile indexFile

  pkgIndex <- evaluate $ PackageIndex.fromList
    [ AvailablePackage {
        packageInfoId      = pkgid,
        packageDescription = pkg,
        packageSource      = RepoTarballPackage repo
      }
    | (pkgid, pkg) <- pkgs]

  return (pkgIndex, prefs)

  where
    extract (pkgs, prefs) entry = fromMaybe (pkgs, prefs) $
              (do pkg <- extractPkg entry; return (pkg:pkgs, prefs))
      `mplus` (do prefs' <- extractPrefs entry; return (pkgs, prefs'++prefs))

    extractPrefs :: Tar.Entry -> Maybe [Dependency]
    extractPrefs entry
      | takeFileName (Tar.fileName entry) == "preferred-versions"
      = Just . parsePreferredVersions
      . BS.Char8.unpack . Tar.fileContent $ entry
      | otherwise = Nothing

    handleNotFound action = catch action $ \e -> if isDoesNotExistError e
      then do
        case repoKind repo of
          Left  remoteRepo -> warn verbosity $
               "The package list for '" ++ remoteRepoName remoteRepo
            ++ "' does not exist. Run 'cabal update' to download it."
          Right _localRepo -> warn verbosity $
               "The package list for the local repo '" ++ repoLocalDir repo
            ++ "' is missing. The repo is invalid."
        return mempty
      else ioError e

parsePreferredVersions :: String -> [Dependency]
parsePreferredVersions = catMaybes
                       . map simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines

-- | Read a compressed \"00-index.tar.gz\" file into a 'PackageIndex'.
--
-- This is supposed to be an \"all in one\" way to easily get at the info in
-- the hackage package index.
--
-- It takes a function to map a 'GenericPackageDescription' into any more
-- specific instance of 'Package' that you might want to use. In the simple
-- case you can just use @\_ p -> p@ here.
--
readPackageIndexFile :: Package pkg
                     => (PackageId -> GenericPackageDescription -> pkg)
                     -> FilePath -> IO (PackageIndex pkg)
readPackageIndexFile mkPkg indexFile = do
  pkgs <- either fail return
        . parseRepoIndex
        . GZip.decompress
      =<< BS.readFile indexFile
  
  evaluate $ PackageIndex.fromList
   [ mkPkg pkgid pkg | (pkgid, pkg) <- pkgs]

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
--
parseRepoIndex :: ByteString
               -> Either String [(PackageId, GenericPackageDescription)]
parseRepoIndex = foldlTarball (\pkgs -> maybe pkgs (:pkgs) . extractPkg) []

extractPkg :: Tar.Entry -> Maybe (PackageId, GenericPackageDescription)
extractPkg entry
  | takeExtension fileName == ".cabal"
  = case splitDirectories (normalise fileName) of
      [pkgname,vers,_] -> case simpleParse vers of
        Just ver -> Just (pkgid, descr)
          where
            pkgid  = PackageIdentifier (PackageName pkgname) ver
            parsed = parsePackageDescription . fromUTF8 . BS.Char8.unpack
                                             . Tar.fileContent $ entry
            descr  = case parsed of
              ParseOk _ d -> d
              _           -> error $ "Couldn't read cabal file "
                                  ++ show fileName
        _ -> Nothing
      _ -> Nothing
  | otherwise = Nothing
  where
    fileName = Tar.fileName entry

foldlTarball :: (a -> Tar.Entry -> a) -> a
             -> ByteString -> Either String a
foldlTarball f z = either Left (Right . foldl f z) . check [] . Tar.read
  where
    check _  (Tar.Fail err)  = Left  err
    check ok Tar.Done        = Right ok
    check ok (Tar.Next e es) = check (e:ok) es

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
              then "There is no package named " ++ display name
              else "The package name " ++ display name ++ "is ambigious. "
                ++ "It could be: " ++ intercalate ", " (map display matches)
          | (name, matches) <- ambigious ]

-- | Given an index of known packages and a package name, figure out which one it
-- might be referring to. If there is an exact case-sensitive match then that's
-- ok. If it matches just one package case-insensitively then that's also ok.
-- The only problem is if it matches multiple packages case-insensitively, in
-- that case it is ambigious.
--
disambiguatePackageName :: PackageIndex AvailablePackage
                        -> PackageName
                        -> Either PackageName [PackageName]
disambiguatePackageName index (PackageName name) =
    case PackageIndex.searchByName index name of
      PackageIndex.None              -> Right []
      PackageIndex.Unambiguous pkgs  -> Left (pkgName (packageId (head pkgs)))
      PackageIndex.Ambiguous   pkgss -> Right [ pkgName (packageId pkg)
                                           | (pkg:_) <- pkgss ]

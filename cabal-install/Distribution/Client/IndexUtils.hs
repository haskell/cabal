-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Distribution.Client.IndexUtils (
  getInstalledPackages,
  getAvailablePackages,

  readPackageIndexFile,
  parseRepoIndex,

  disambiguatePackageName,
  disambiguateDependencies
  ) where

import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types

import Distribution.Package
         ( PackageId, PackageIdentifier(..), PackageName(..), Package(..)
         , Dependency(Dependency), InstalledPackageId(..) )
import Distribution.Client.PackageIndex (PackageIndex)
import qualified Distribution.Client.PackageIndex as PackageIndex
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.PackageDescription
         ( GenericPackageDescription )
import Distribution.PackageDescription.Parse
         ( parsePackageDescription )
import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import qualified Distribution.Simple.Configure as Configure
         ( getInstalledPackages )
import Distribution.ParseUtils
         ( ParseResult(..) )
import Distribution.Version
         ( Version(Version), intersectVersionRanges )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Verbosity (Verbosity)
import Distribution.Simple.Utils (die, warn, info, intercalate, fromUTF8)

import Data.Maybe  (catMaybes, fromMaybe)
import Data.List   (isPrefixOf, find)
import Data.Monoid (Monoid(..))
import qualified Data.Map as Map
import Control.Monad (MonadPlus(mplus), when)
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO.Error (isDoesNotExistError)
import System.Directory
         ( getModificationTime )
import System.Time
         ( getClockTime, diffClockTimes, normalizeTimeDiff, TimeDiff(tdDay) )

getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO (PackageIndex InstalledPackage)
getInstalledPackages verbosity comp packageDbs conf =
  fmap convert (Configure.getInstalledPackages verbosity comp packageDbs conf)
  where
    convert :: InstalledPackageIndex.PackageIndex -> PackageIndex InstalledPackage
    convert index = PackageIndex.fromList $
      reverse -- because later ones mask earlier ones, but
              -- InstalledPackageIndex.allPackages gives us the most preferred
              -- instances first, when packages share a package id, like when
              -- the same package is installed in the global & user dbs.
      [ InstalledPackage ipkg (sourceDeps index ipkg)
      | ipkg <- InstalledPackageIndex.allPackages index ]

    -- The InstalledPackageInfo only lists dependencies by the
    -- InstalledPackageId, which means we do not directly know the corresponding
    -- source dependency. The only way to find out is to lookup the
    -- InstalledPackageId to get the InstalledPackageInfo and look at its
    -- source PackageId. But if the package is broken because it depends on
    -- other packages that do not exist then we have a problem we cannot find
    -- the original source package id. Instead we make up a bogus package id.
    -- This should have the same effect since it should be a dependency on a
    -- non-existant package.
    sourceDeps index ipkg =
      [ maybe (brokenPackageId depid) packageId mdep
      | let depids = InstalledPackageInfo.depends ipkg
            getpkg = InstalledPackageIndex.lookupInstalledPackageId index
      , (depid, mdep) <- zip depids (map getpkg depids) ]

    brokenPackageId (InstalledPackageId str) =
      PackageIdentifier (PackageName (str ++ "-broken")) (Version [] [])

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'AvailablePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
getAvailablePackages :: Verbosity -> [Repo] -> IO AvailablePackageDb
getAvailablePackages verbosity [] = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
  return AvailablePackageDb {
    packageIndex       = mempty,
    packagePreferences = mempty
  }
getAvailablePackages verbosity repos = do
  info verbosity "Reading available packages..."
  pkgss <- mapM (readRepoIndex verbosity) repos
  let (pkgs, prefs) = mconcat pkgss
      prefs' = Map.fromListWith intersectVersionRanges
                 [ (name, range) | Dependency name range <- prefs ]
  _ <- evaluate pkgs
  _ <- evaluate prefs'
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

  warnIfIndexIsOld indexFile
  return (pkgIndex, prefs)

  where
    extract (pkgs, prefs) entry = fromMaybe (pkgs, prefs) $
              (do pkg <- extractPkg entry; return (pkg:pkgs, prefs))
      `mplus` (do prefs' <- extractPrefs entry; return (pkgs, prefs'++prefs))

    extractPrefs :: Tar.Entry -> Maybe [Dependency]
    extractPrefs entry = case Tar.entryContent entry of
      Tar.NormalFile content _
         | takeFileName (Tar.entryPath entry) == "preferred-versions"
        -> Just . parsePreferredVersions
         . BS.Char8.unpack $ content
      _ -> Nothing

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

    isOldThreshold = 15 --days
    warnIfIndexIsOld indexFile = do
      indexTime   <- getModificationTime indexFile
      currentTime <- getClockTime
      let diff = normalizeTimeDiff (diffClockTimes currentTime indexTime)
      when (tdDay diff >= isOldThreshold) $ case repoKind repo of
        Left  remoteRepo -> warn verbosity $
             "The package list for '" ++ remoteRepoName remoteRepo
          ++ "' is " ++ show (tdDay diff)  ++ " days old.\nRun "
          ++ "'cabal update' to get the latest list of available packages."
        Right _localRepo -> return ()

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
        . maybeDecompress
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
extractPkg entry = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeExtension fileName == ".cabal"
    -> case splitDirectories (normalise fileName) of
        [pkgname,vers,_] -> case simpleParse vers of
          Just ver -> Just (pkgid, descr)
            where
              pkgid  = PackageIdentifier (PackageName pkgname) ver
              parsed = parsePackageDescription . fromUTF8 . BS.Char8.unpack
                                               $ content
              descr  = case parsed of
                ParseOk _ d -> d
                _           -> error $ "Couldn't read cabal file "
                                    ++ show fileName
          _ -> Nothing
        _ -> Nothing
  _ -> Nothing
  where
    fileName = Tar.entryPath entry

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
              then "There is no package named " ++ display name ++ ". "
                ++ "Perhaps you need to run 'cabal update' first?"
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
disambiguatePackageName index pkgname@(PackageName name) =
    case checkAmbiguity pkgname (map fst $ PackageIndex.searchByName index name) of
      None               -> Right []
      Unambiguous name'  -> Left name'
      Ambiguous   names' -> Right names'

checkAmbiguity :: PackageName -> [PackageName] -> MaybeAmbigious PackageName
checkAmbiguity name names =
    case names of
      []           -> None
      [name']      -> Unambiguous name'
      _            -> case find (name==) names of
                        Just name' -> Unambiguous name'
                        Nothing    -> Ambiguous names

data MaybeAmbigious a = None | Unambiguous a | Ambiguous [a]

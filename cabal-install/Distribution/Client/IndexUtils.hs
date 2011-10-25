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
  getSourcePackages,

  readPackageIndexFile,
  parsePackageIndex,
  readRepoIndex,
  updateRepoIndexCache,
  ) where

import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types

import Distribution.Package
         ( PackageId, PackageIdentifier(..), PackageName(..)
         , Package(..), packageVersion, packageName
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
import Distribution.Verbosity
         ( Verbosity, lessVerbose )
import Distribution.Simple.Utils
         ( die, warn, info, fromUTF8, equating )

import Data.Char   (isAlphaNum)
import Data.Maybe  (catMaybes, fromMaybe)
import Data.List   (isPrefixOf, groupBy)
import Data.Monoid (Monoid(..))
import qualified Data.Map as Map
import Control.Monad (MonadPlus(mplus), when, unless, liftM)
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Lazy (ByteString)
import Distribution.Client.GZipUtils (maybeDecompress)
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error (isDoesNotExistError)
import System.Directory
         ( getModificationTime, doesFileExist )
import System.Time
         ( getClockTime, diffClockTimes, normalizeTimeDiff, TimeDiff(tdDay) )


getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex.PackageIndex
getInstalledPackages verbosity comp packageDbs conf =
    Configure.getInstalledPackages verbosity' comp packageDbs conf
  where
    --FIXME: make getInstalledPackages use sensible verbosity in the first place
    verbosity'  = lessVerbose verbosity

------------------------------------------------------------------------
-- Reading the source package index
--

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'SourcePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
getSourcePackages :: Verbosity -> [Repo] -> IO SourcePackageDb
getSourcePackages verbosity [] = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
  return SourcePackageDb {
    packageIndex       = mempty,
    packagePreferences = mempty
  }
getSourcePackages verbosity repos = do
  info verbosity "Reading available packages..."
  pkgss <- mapM (readRepoIndex verbosity) repos
  let (pkgs, prefs) = mconcat pkgss
      prefs' = Map.fromListWith intersectVersionRanges
                 [ (name, range) | Dependency name range <- prefs ]
  _ <- evaluate pkgs
  _ <- evaluate prefs'
  return SourcePackageDb {
    packageIndex       = pkgs,
    packagePreferences = prefs'
  }


-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'SourcePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
readRepoIndex :: Verbosity -> Repo
              -> IO (PackageIndex SourcePackage, [Dependency])
readRepoIndex verbosity repo =
  let indexFile = repoLocalDir repo </> "00-index.tar"
      cacheFile = repoLocalDir repo </> "00-index.cache"
  in handleNotFound $ do
    warnIfIndexIsOld indexFile
    whenCacheOutOfDate indexFile cacheFile $ do
      info verbosity $ "Updating the index cache file..."
      updatePackageIndexCacheFile indexFile cacheFile
    readPackageIndexCacheFile mkAvailablePackage indexFile cacheFile

  where
    mkAvailablePackage pkgid pkg =
      SourcePackage {
        packageInfoId      = pkgid,
        packageDescription = pkg,
        packageSource      = RepoTarballPackage repo pkgid Nothing
      }

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

-- | It is not necessary to call this, as the cache will be updated when the
-- index is read normally. However you can do the work earlier if you like.
--
updateRepoIndexCache :: Verbosity -> Repo -> IO ()
updateRepoIndexCache verbosity repo =
    whenCacheOutOfDate indexFile cacheFile $ do
      info verbosity $ "Updating the index cache file..."
      updatePackageIndexCacheFile indexFile cacheFile
  where
    indexFile = repoLocalDir repo </> "00-index.tar"
    cacheFile = repoLocalDir repo </> "00-index.cache"

whenCacheOutOfDate :: FilePath-> FilePath -> IO () -> IO ()
whenCacheOutOfDate origFile cacheFile action = do
  exists <- doesFileExist cacheFile
  if not exists
    then action
    else do
      origTime  <- getModificationTime origFile
      cacheTime <- getModificationTime cacheFile
      unless (cacheTime >= origTime) action


------------------------------------------------------------------------
-- Reading the index file
--

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
                     -> FilePath
                     -> IO (PackageIndex pkg, [Dependency])
readPackageIndexFile mkPkg indexFile = do
  (pkgs, prefs) <- either fail return
                 . parsePackageIndex
                 . maybeDecompress
               =<< BS.readFile indexFile

  pkgs' <- evaluate $ PackageIndex.fromList
            [ mkPkg pkgid pkg | (pkgid, pkg, _) <- pkgs]
  return (pkgs', prefs)

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
--
parsePackageIndex :: ByteString
                  -> Either String
                            ( [(PackageId, GenericPackageDescription, BlockNo)]
                            , [Dependency] )
parsePackageIndex = accum 0 [] [] . Tar.read
  where
    accum blockNo pkgs prefs es = case es of
      Tar.Fail err   -> Left  err
      Tar.Done       -> Right (reverse pkgs, reverse prefs)
      Tar.Next e es' -> accum blockNo' pkgs' prefs' es'
        where
          (pkgs', prefs') = extract blockNo pkgs prefs e
          blockNo'        = blockNo + sizeInBlocks e

    extract blockNo pkgs prefs entry =
       fromMaybe (pkgs, prefs) $
                 tryExtractPkg
         `mplus` tryExtractPrefs
      where
        tryExtractPkg = do
          (pkgid, pkg) <- extractPkg entry
          return ((pkgid, pkg, blockNo):pkgs, prefs)

        tryExtractPrefs = do
          prefs' <- extractPrefs entry
          return (pkgs, prefs'++prefs)

    sizeInBlocks entry =
        1 + case Tar.entryContent entry of
              Tar.NormalFile     _   size -> bytesToBlocks size
              Tar.OtherEntryType _ _ size -> bytesToBlocks size
              _                           -> 0
      where
        bytesToBlocks s = 1 + ((fromIntegral s - 1) `div` 512)

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

extractPrefs :: Tar.Entry -> Maybe [Dependency]
extractPrefs entry = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeFileName (Tar.entryPath entry) == "preferred-versions"
    -> Just . parsePreferredVersions
     . BS.Char8.unpack $ content
  _ -> Nothing

parsePreferredVersions :: String -> [Dependency]
parsePreferredVersions = catMaybes
                       . map simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines

------------------------------------------------------------------------
-- Reading and updating the index cache
--

updatePackageIndexCacheFile :: FilePath -> FilePath -> IO ()
updatePackageIndexCacheFile indexFile cacheFile = do
    (pkgs, prefs) <- either fail return
                   . parsePackageIndex
                   . maybeDecompress
                 =<< BS.readFile indexFile
    let cache = mkCache pkgs prefs
    writeFile cacheFile (showIndexCache cache)
  where
    mkCache pkgs prefs =
        [ CachePrefrence pref          | pref <- prefs ]
     ++ [ CachePackageId pkgid blockNo | (pkgid, _, blockNo) <- pkgs ]

readPackageIndexCacheFile :: Package pkg
                          => (PackageId -> GenericPackageDescription -> pkg)
                          -> FilePath
                          -> FilePath
                          -> IO (PackageIndex pkg, [Dependency])
readPackageIndexCacheFile mkPkg indexFile cacheFile = do
  indexHnd <- openFile indexFile ReadMode
  cache    <- liftM readIndexCache (BSS.readFile cacheFile)
  packageIndexFromCache mkPkg indexHnd cache


packageIndexFromCache :: Package pkg
                      => (PackageId -> GenericPackageDescription -> pkg)
                      -> Handle
                      -> [IndexCacheEntry]
                      -> IO (PackageIndex pkg, [Dependency])
packageIndexFromCache mkPkg hnd = accum mempty []
  where
    accum srcpkgs prefs [] = do
      -- Have to reverse entries, since in a tar file, later entries mask
      -- earlier ones, and PackageIndex.fromList does the same, but we
      -- accumulate the list of entries in reverse order, so need to reverse.
      pkgIndex <- evaluate $ PackageIndex.fromList (reverse srcpkgs)
      return (pkgIndex, prefs)

    accum srcpkgs prefs (CachePackageId pkgid blockno : entries) = do
      -- Given the cache entry, make a package index entry.
      -- The magic here is that we use lazy IO to read the .cabal file
      -- from the index tarball if it turns out that we need it.
      -- Most of the time we only need the package id.
      pkg <- unsafeInterleaveIO $ do
               getPackageDescription blockno
      let srcpkg = mkPkg pkgid pkg
      accum (srcpkg:srcpkgs) prefs entries

    accum srcpkgs prefs (CachePrefrence pref : entries) =
      accum srcpkgs (pref:prefs) entries

    getPackageDescription blockno = do
      hSeek hnd AbsoluteSeek (fromIntegral (blockno * 512))
      header  <- BS.hGet hnd 512
      size    <- getEntrySize header
      content <- BS.hGet hnd (fromIntegral size)
      readPackageDescription content

    getEntrySize header =
      case Tar.read header of
        Tar.Next e _ ->
          case Tar.entryContent e of
            Tar.NormalFile _ size -> return size
            _                     -> interror "unexpected tar entry type"
        _ -> interror "could not read tar file entry"

    readPackageDescription content =
      case parsePackageDescription . fromUTF8 . BS.Char8.unpack $ content of
        ParseOk _ d -> return d
        _           -> interror "failed to parse .cabal file"

    interror msg = die $ "internal error when reading package index: " ++ msg
                      ++ "The package index or index cache is probably "
                      ++ "corrupt. Running cabal update might fix it."

------------------------------------------------------------------------
-- Index cache data structure
--

-- | Tar files are block structured with 512 byte blocks. Every header and file
-- content starts on a block boundary.
--
type BlockNo = Int

data IndexCacheEntry = CachePackageId PackageId BlockNo
                     | CachePrefrence Dependency
  deriving (Eq, Show)

readIndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
readIndexCacheEntry = \line ->
  case BSS.words line of
    [key, pkgnamestr, pkgverstr, sep, blocknostr]
      | key == packageKey && sep == blocknoKey ->
      case (parseName pkgnamestr, parseVer pkgverstr [], parseBlockNo blocknostr) of
        (Just pkgname, Just pkgver, Just blockno)
          -> Just (CachePackageId (PackageIdentifier pkgname pkgver) blockno)
        _ -> Nothing
    (key: remainder) | key == preferredVersionKey ->
      fmap CachePrefrence (simpleParse (BSS.unpack (BSS.unwords remainder)))
    _  -> Nothing
  where
    packageKey = BSS.pack "pkg:"
    blocknoKey = BSS.pack "b#"
    preferredVersionKey = BSS.pack "pref-ver:"

    parseName str
      | BSS.all (\c -> isAlphaNum c || c == '-') str
                  = Just (PackageName (BSS.unpack str))
      | otherwise = Nothing

    parseVer str vs =
      case BSS.readInt str of
        Nothing        -> Nothing
        Just (v, str') -> case BSS.uncons str' of
          Just ('.', str'') -> parseVer str'' (v:vs)
          Just _            -> Nothing
          Nothing           -> Just (Version (reverse (v:vs)) [])

    parseBlockNo str =
      case BSS.readInt str of
        Just (blockno, remainder) | BSS.null remainder -> Just blockno
        _                                              -> Nothing

showIndexCacheEntry :: IndexCacheEntry -> String
showIndexCacheEntry entry = case entry of
   CachePackageId pkgid b -> "pkg: " ++ display (packageName pkgid)
                                  ++ " " ++ display (packageVersion pkgid)
                          ++ " b# " ++ show b
   CachePrefrence dep     -> "pref-ver: " ++ display dep

readIndexCache :: BSS.ByteString -> [IndexCacheEntry]
readIndexCache = catMaybes . map readIndexCacheEntry . BSS.lines

showIndexCache :: [IndexCacheEntry] -> String
showIndexCache = unlines . map showIndexCacheEntry

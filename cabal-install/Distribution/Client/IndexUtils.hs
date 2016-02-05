{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
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
  getIndexFileAge,
  getInstalledPackages,
  Configure.getInstalledPackagesMonitorFiles,
  getSourcePackages,
  getSourcePackagesMonitorFiles,

  Index(..),
  PackageEntry(..),
  parsePackageIndex,
  updateRepoIndexCache,
  updatePackageIndexCacheFile,
  readCacheStrict,

  BuildTreeRefType(..), refTypeFromTypeCode, typeCodeFromRefType
  ) where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types

import Distribution.Package
         ( PackageId, PackageIdentifier(..), PackageName(..)
         , Package(..), packageVersion, packageName
         , Dependency(Dependency) )
import Distribution.Client.PackageIndex (PackageIndex)
import qualified Distribution.Client.PackageIndex      as PackageIndex
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.PackageDescription.Parse as PackageDesc.Parse
import Distribution.PackageDescription
         ( GenericPackageDescription )
import Distribution.PackageDescription.Parse
         ( parsePackageDescription )
import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration )
import qualified Distribution.Simple.Configure as Configure
         ( getInstalledPackages, getInstalledPackagesMonitorFiles )
import Distribution.ParseUtils
         ( ParseResult(..) )
import Distribution.Version
         ( Version(Version), intersectVersionRanges )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Verbosity
         ( Verbosity, normal, lessVerbose )
import Distribution.Simple.Utils
         ( die, warn, info, fromUTF8, ignoreBOM )
import Distribution.Client.Setup
         ( RepoContext(..) )

import Data.Char   (isAlphaNum)
import Data.Maybe  (mapMaybe, catMaybes, maybeToList)
import Data.List   (isPrefixOf)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(..))
#endif
import qualified Data.Map as Map
import Control.Monad (when, liftM)
import Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Lazy (ByteString)
import Distribution.Client.GZipUtils (maybeDecompress)
import Distribution.Client.Utils ( byteStringToFilePath
                                 , tryFindAddSourcePackageDesc )
import Distribution.Compat.Exception (catchIO)
import Distribution.Client.Compat.Time (getFileAge, getModTime)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath
         ( (</>), takeExtension, replaceExtension, splitDirectories, normalise )
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error (isDoesNotExistError)

import qualified Hackage.Security.Client    as Sec
import qualified Hackage.Security.Util.Some as Sec

-- | Reduced-verbosity version of 'Configure.getInstalledPackages'
getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packageDbs conf =
    Configure.getInstalledPackages verbosity' comp packageDbs conf
  where
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
getSourcePackages :: Verbosity -> RepoContext -> IO SourcePackageDb
getSourcePackages verbosity repoCtxt | null (repoContextRepos repoCtxt) = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
  return SourcePackageDb {
    packageIndex       = mempty,
    packagePreferences = mempty
  }
getSourcePackages verbosity repoCtxt = do
  info verbosity "Reading available packages..."
  pkgss <- mapM (\r -> readRepoIndex verbosity repoCtxt r) (repoContextRepos repoCtxt)
  let (pkgs, prefs) = mconcat pkgss
      prefs' = Map.fromListWith intersectVersionRanges
                 [ (name, range) | Dependency name range <- prefs ]
  _ <- evaluate pkgs
  _ <- evaluate prefs'
  return SourcePackageDb {
    packageIndex       = pkgs,
    packagePreferences = prefs'
  }

readCacheStrict :: Verbosity -> Index -> (PackageEntry -> pkg) -> IO ([pkg], [Dependency])
readCacheStrict verbosity index mkPkg = do
    updateRepoIndexCache verbosity index
    cache <- liftM readIndexCache $ BSS.readFile (cacheFile index)
    withFile (indexFile index) ReadMode $ \indexHnd ->
      packageListFromCache mkPkg indexHnd cache ReadPackageIndexStrict

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'SourcePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
readRepoIndex :: Verbosity -> RepoContext -> Repo
              -> IO (PackageIndex SourcePackage, [Dependency])
readRepoIndex verbosity repoCtxt repo =
  handleNotFound $ do
    warnIfIndexIsOld =<< getIndexFileAge repo
    updateRepoIndexCache verbosity (RepoIndex repoCtxt repo)
    readPackageIndexCacheFile mkAvailablePackage (RepoIndex repoCtxt repo)

  where
    mkAvailablePackage pkgEntry =
      SourcePackage {
        packageInfoId      = pkgid,
        packageDescription = packageDesc pkgEntry,
        packageSource      = case pkgEntry of
          NormalPackage _ _ _ _       -> RepoTarballPackage repo pkgid Nothing
          BuildTreeRef  _  _ _ path _ -> LocalUnpackedPackage path,
        packageDescrOverride = case pkgEntry of
          NormalPackage _ _ pkgtxt _ -> Just pkgtxt
          _                          -> Nothing
      }
      where
        pkgid = packageId pkgEntry

    handleNotFound action = catchIO action $ \e -> if isDoesNotExistError e
      then do
        case repo of
          RepoRemote{..} -> warn verbosity $ errMissingPackageList repoRemote
          RepoSecure{..} -> warn verbosity $ errMissingPackageList repoRemote
          RepoLocal{..}  -> warn verbosity $
               "The package list for the local repo '" ++ repoLocalDir
            ++ "' is missing. The repo is invalid."
        return mempty
      else ioError e

    isOldThreshold = 15 --days
    warnIfIndexIsOld dt = do
      when (dt >= isOldThreshold) $ case repo of
        RepoRemote{..} -> warn verbosity $ errOutdatedPackageList repoRemote dt
        RepoSecure{..} -> warn verbosity $ errOutdatedPackageList repoRemote dt
        RepoLocal{..}  -> return ()

    errMissingPackageList repoRemote =
         "The package list for '" ++ remoteRepoName repoRemote
      ++ "' does not exist. Run 'cabal update' to download it."
    errOutdatedPackageList repoRemote dt =
         "The package list for '" ++ remoteRepoName repoRemote
      ++ "' is " ++ shows (floor dt :: Int) " days old.\nRun "
      ++ "'cabal update' to get the latest list of available packages."

-- | Return the age of the index file in days (as a Double).
getIndexFileAge :: Repo -> IO Double
getIndexFileAge repo = getFileAge $ repoLocalDir repo </> "00-index.tar"

-- | A set of files (or directories) that can be monitored to detect when
-- there might have been a change in the source packages.
--
getSourcePackagesMonitorFiles :: [Repo] -> [FilePath]
getSourcePackagesMonitorFiles repos =
    [ repoLocalDir repo </> "00-index.cache"
    | repo <- repos ]

-- | It is not necessary to call this, as the cache will be updated when the
-- index is read normally. However you can do the work earlier if you like.
--
updateRepoIndexCache :: Verbosity -> Index -> IO ()
updateRepoIndexCache verbosity index =
    whenCacheOutOfDate index $ do
      updatePackageIndexCacheFile verbosity index

whenCacheOutOfDate :: Index -> IO () -> IO ()
whenCacheOutOfDate index action = do
  exists <- doesFileExist $ cacheFile index
  if not exists
    then action
    else do
      indexTime <- getModTime $ indexFile index
      cacheTime <- getModTime $ cacheFile index
      when (indexTime > cacheTime) action

------------------------------------------------------------------------
-- Reading the index file
--

-- | An index entry is either a normal package, or a local build tree reference.
data PackageEntry =
  NormalPackage  PackageId GenericPackageDescription ByteString BlockNo
  | BuildTreeRef BuildTreeRefType
                 PackageId GenericPackageDescription FilePath   BlockNo

-- | A build tree reference is either a link or a snapshot.
data BuildTreeRefType = SnapshotRef | LinkRef
                      deriving Eq

refTypeFromTypeCode :: Tar.TypeCode -> BuildTreeRefType
refTypeFromTypeCode t
  | t == Tar.buildTreeRefTypeCode      = LinkRef
  | t == Tar.buildTreeSnapshotTypeCode = SnapshotRef
  | otherwise                          =
    error "Distribution.Client.IndexUtils.refTypeFromTypeCode: unknown type code"

typeCodeFromRefType :: BuildTreeRefType -> Tar.TypeCode
typeCodeFromRefType LinkRef     = Tar.buildTreeRefTypeCode
typeCodeFromRefType SnapshotRef = Tar.buildTreeSnapshotTypeCode

instance Package PackageEntry where
  packageId (NormalPackage  pkgid _ _ _) = pkgid
  packageId (BuildTreeRef _ pkgid _ _ _) = pkgid

packageDesc :: PackageEntry -> GenericPackageDescription
packageDesc (NormalPackage  _ descr _ _) = descr
packageDesc (BuildTreeRef _ _ descr _ _) = descr

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
--

data PackageOrDep = Pkg PackageEntry | Dep Dependency

-- | Read @00-index.tar.gz@ and extract @.cabal@ and @preferred-versions@ files
--
-- We read the index using 'Tar.read', which gives us a lazily constructed
-- 'TarEntries'. We translate it to a list of entries using  'tarEntriesList',
-- which preserves the lazy nature of 'TarEntries', and finally 'concatMap' a
-- function over this to translate it to a list of IO actions returning
-- 'PackageOrDep's. We can use 'lazySequence' to turn this into a list of
-- 'PackageOrDep's, still maintaining the lazy nature of the original tar read.
parsePackageIndex :: ByteString -> [IO (Maybe PackageOrDep)]
parsePackageIndex = concatMap (uncurry extract) . tarEntriesList . Tar.read
  where
    extract :: BlockNo -> Tar.Entry -> [IO (Maybe PackageOrDep)]
    extract blockNo entry = tryExtractPkg ++ tryExtractPrefs
      where
        tryExtractPkg = do
          mkPkgEntry <- maybeToList $ extractPkg entry blockNo
          return $ fmap (fmap Pkg) mkPkgEntry

        tryExtractPrefs = do
          prefs' <- maybeToList $ extractPrefs entry
          fmap (return . Just . Dep) prefs'

-- | Turn the 'Entries' data structure from the @tar@ package into a list,
-- and pair each entry with its block number.
--
-- NOTE: This preserves the lazy nature of 'Entries': the tar file is only read
-- as far as the list is evaluated.
tarEntriesList :: Show e => Tar.Entries e -> [(BlockNo, Tar.Entry)]
tarEntriesList = go 0
  where
    go !_ Tar.Done         = []
    go !_ (Tar.Fail e)     = error ("tarEntriesList: " ++ show e)
    go !n (Tar.Next e es') = (n, e) : go (Tar.nextEntryOffset e n) es'

extractPkg :: Tar.Entry -> BlockNo -> Maybe (IO (Maybe PackageEntry))
extractPkg entry blockNo = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeExtension fileName == ".cabal"
    -> case splitDirectories (normalise fileName) of
        [pkgname,vers,_] -> case simpleParse vers of
          Just ver -> Just . return $ Just (NormalPackage pkgid descr content blockNo)
            where
              pkgid  = PackageIdentifier (PackageName pkgname) ver
              parsed = parsePackageDescription . ignoreBOM . fromUTF8 . BS.Char8.unpack
                                               $ content
              descr  = case parsed of
                ParseOk _ d -> d
                _           -> error $ "Couldn't read cabal file "
                                    ++ show fileName
          _ -> Nothing
        _ -> Nothing

  Tar.OtherEntryType typeCode content _
    | Tar.isBuildTreeRefTypeCode typeCode ->
      Just $ do
        let path = byteStringToFilePath content
        dirExists <- doesDirectoryExist path
        result <- if not dirExists then return Nothing
                  else do
                    cabalFile <- tryFindAddSourcePackageDesc path "Error reading package index."
                    descr     <- PackageDesc.Parse.readPackageDescription normal cabalFile
                    return . Just $ BuildTreeRef (refTypeFromTypeCode typeCode) (packageId descr)
                                                 descr path blockNo
        return result

  _ -> Nothing

  where
    fileName = Tar.entryPath entry

extractPrefs :: Tar.Entry -> Maybe [Dependency]
extractPrefs entry = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeFileName entrypath == "preferred-versions"
    -> Just prefs
    where
      entrypath = Tar.entryPath entry
      prefs     = parsePreferredVersions content
  _ -> Nothing

parsePreferredVersions :: ByteString -> [Dependency]
parsePreferredVersions = mapMaybe simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines
                       . BS.Char8.unpack -- TODO: Are we sure no unicode?

------------------------------------------------------------------------
-- Reading and updating the index cache
--

-- | Variation on 'sequence' which evaluates the actions lazily
--
-- Pattern matching on the result list will execute just the first action;
-- more generally pattern matching on the first @n@ '(:)' nodes will execute
-- the first @n@ actions.
lazySequence :: [IO a] -> IO [a]
lazySequence = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do x'  <- x
                   xs' <- lazySequence xs
                   return (x' : xs')

-- | Which index do we mean?
data Index =
    -- | The main index for the specified repository
    RepoIndex RepoContext Repo

    -- | A sandbox-local repository
    -- Argument is the location of the index file
  | SandboxIndex FilePath

indexFile :: Index -> FilePath
indexFile (RepoIndex _ctxt repo) = repoLocalDir repo </> "00-index.tar"
indexFile (SandboxIndex index)   = index

cacheFile :: Index -> FilePath
cacheFile (RepoIndex _ctxt repo) = repoLocalDir repo </> "00-index.cache"
cacheFile (SandboxIndex index)   = index `replaceExtension` "cache"

updatePackageIndexCacheFile :: Verbosity -> Index -> IO ()
updatePackageIndexCacheFile verbosity index = do
    info verbosity ("Updating index cache file " ++ cacheFile index)
    withIndexEntries index $ \entries -> do
      let cache = Cache { cacheEntries = entries }
      writeFile (cacheFile index) (showIndexCache cache)

-- | Read the index (for the purpose of building a cache)
--
-- The callback is provided with list of cache entries, which is guaranteed to
-- be lazily constructed. This list must ONLY be used in the scope of the
-- callback; when the callback is terminated the file handle to the index will
-- be closed and further attempts to read from the list will result in (pure)
-- I/O exceptions.
--
-- In the construction of the index for a secure repo we take advantage of the
-- index built by the @hackage-security@ library to avoid reading the @.tar@
-- file as much as possible (we need to read it only to extract preferred
-- versions). This helps performance, but is also required for correctness:
-- the new @01-index.tar.gz@ may have multiple versions of preferred-versions
-- files, and 'parsePackageIndex' does not correctly deal with that (see #2956);
-- by reading the already-built cache from the security library we will be sure
-- to only read the latest versions of all files.
--
-- TODO: It would be nicer if we actually incrementally updated @cabal@'s
-- cache, rather than reconstruct it from zero on each update. However, this
-- would require a change in the cache format.
withIndexEntries :: Index -> ([IndexCacheEntry] -> IO a) -> IO a
withIndexEntries (RepoIndex repoCtxt repo@RepoSecure{..}) callback =
    repoContextWithSecureRepo repoCtxt repo $ \repoSecure ->
      Sec.withIndex repoSecure $ \Sec.IndexCallbacks{..} -> do
        let mk :: (Sec.DirectoryEntry, fp, Maybe (Sec.Some Sec.IndexFile))
               -> IO [IndexCacheEntry]
            mk (_, _fp, Nothing) =
              return [] -- skip unrecognized file
            mk (_, _fp, Just (Sec.Some (Sec.IndexPkgMetadata _pkgId))) =
              return [] -- skip metadata
            mk (dirEntry, _fp, Just (Sec.Some (Sec.IndexPkgCabal pkgId))) = do
              let blockNo = fromIntegral (Sec.directoryEntryBlockNo dirEntry)
              return [CachePackageId pkgId blockNo]
            mk (dirEntry, _fp, Just (Sec.Some file@(Sec.IndexPkgPrefs _pkgName))) = do
              content <- Sec.indexEntryContent `fmap` indexLookupFileEntry dirEntry file
              return $ map CachePreference (parsePreferredVersions content)
        entriess <- lazySequence $ map mk (Sec.directoryEntries indexDirectory)
        callback $ concat entriess
withIndexEntries index callback = do
    withFile (indexFile index) ReadMode $ \h -> do
      bs          <- maybeDecompress `fmap` BS.hGetContents h
      pkgsOrPrefs <- lazySequence $ parsePackageIndex bs
      callback $ map toCache (catMaybes pkgsOrPrefs)
  where
    toCache :: PackageOrDep -> IndexCacheEntry
    toCache (Pkg (NormalPackage pkgid _ _ blockNo)) = CachePackageId pkgid blockNo
    toCache (Pkg (BuildTreeRef refType _ _ _ blockNo)) = CacheBuildTreeRef refType blockNo
    toCache (Dep d) = CachePreference d

data ReadPackageIndexMode = ReadPackageIndexStrict
                          | ReadPackageIndexLazyIO

readPackageIndexCacheFile :: Package pkg
                          => (PackageEntry -> pkg)
                          -> Index
                          -> IO (PackageIndex pkg, [Dependency])
readPackageIndexCacheFile mkPkg index = do
  cache    <- liftM readIndexCache $ BSS.readFile (cacheFile index)
  indexHnd <- openFile (indexFile index) ReadMode
  packageIndexFromCache mkPkg indexHnd cache ReadPackageIndexLazyIO

packageIndexFromCache :: Package pkg
                      => (PackageEntry -> pkg)
                      -> Handle
                      -> Cache
                      -> ReadPackageIndexMode
                      -> IO (PackageIndex pkg, [Dependency])
packageIndexFromCache mkPkg hnd cache mode = do
     (pkgs, prefs) <- packageListFromCache mkPkg hnd cache mode
     pkgIndex <- evaluate $ PackageIndex.fromList pkgs
     return (pkgIndex, prefs)

-- | Read package list
--
-- The result packages (though not the preferences) are guaranteed to be listed
-- in the same order as they are in the tar file (because later entries in a tar
-- file mask earlier ones).
packageListFromCache :: (PackageEntry -> pkg)
                     -> Handle
                     -> Cache
                     -> ReadPackageIndexMode
                     -> IO ([pkg], [Dependency])
packageListFromCache mkPkg hnd Cache{..} mode = accum mempty [] cacheEntries
  where
    accum srcpkgs prefs [] = return (reverse srcpkgs, prefs)

    accum srcpkgs prefs (CachePackageId pkgid blockno : entries) = do
      -- Given the cache entry, make a package index entry.
      -- The magic here is that we use lazy IO to read the .cabal file
      -- from the index tarball if it turns out that we need it.
      -- Most of the time we only need the package id.
      ~(pkg, pkgtxt) <- unsafeInterleaveIO $ do
        pkgtxt <- getEntryContent blockno
        pkg    <- readPackageDescription pkgtxt
        return (pkg, pkgtxt)
      let srcpkg = case mode of
            ReadPackageIndexLazyIO ->
              mkPkg (NormalPackage pkgid pkg pkgtxt blockno)
            ReadPackageIndexStrict ->
              pkg `seq` pkgtxt `seq` mkPkg (NormalPackage pkgid pkg
                                            pkgtxt blockno)
      accum (srcpkg:srcpkgs) prefs entries

    accum srcpkgs prefs (CacheBuildTreeRef refType blockno : entries) = do
      -- We have to read the .cabal file eagerly here because we can't cache the
      -- package id for build tree references - the user might edit the .cabal
      -- file after the reference was added to the index.
      path <- liftM byteStringToFilePath . getEntryContent $ blockno
      pkg  <- do let err = "Error reading package index from cache."
                 file <- tryFindAddSourcePackageDesc path err
                 PackageDesc.Parse.readPackageDescription normal file
      let srcpkg = mkPkg (BuildTreeRef refType (packageId pkg) pkg path blockno)
      accum (srcpkg:srcpkgs) prefs entries

    accum srcpkgs prefs (CachePreference pref : entries) =
      accum srcpkgs (pref:prefs) entries

    getEntryContent :: BlockNo -> IO ByteString
    getEntryContent blockno = do
      entry <- Tar.hReadEntry hnd blockno
      case Tar.entryContent entry of
        Tar.NormalFile content _size -> return content
        Tar.OtherEntryType typecode content _size
          | Tar.isBuildTreeRefTypeCode typecode
          -> return content
        _ -> interror "unexpected tar entry type"

    readPackageDescription :: ByteString -> IO GenericPackageDescription
    readPackageDescription content =
      case parsePackageDescription . ignoreBOM . fromUTF8 . BS.Char8.unpack $ content of
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
type BlockNo = Tar.TarEntryOffset

data IndexCacheEntry = CachePackageId PackageId BlockNo
                     | CacheBuildTreeRef BuildTreeRefType BlockNo
                     | CachePreference Dependency
  deriving (Eq)

installedUnitId, blocknoKey, buildTreeRefKey, preferredVersionKey :: String
installedUnitId = "pkg:"
blocknoKey = "b#"
buildTreeRefKey     = "build-tree-ref:"
preferredVersionKey = "pref-ver:"

readIndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
readIndexCacheEntry = \line ->
  case BSS.words line of
    [key, pkgnamestr, pkgverstr, sep, blocknostr]
      | key == BSS.pack installedUnitId && sep == BSS.pack blocknoKey ->
      case (parseName pkgnamestr, parseVer pkgverstr [],
            parseBlockNo blocknostr) of
        (Just pkgname, Just pkgver, Just blockno)
          -> Just (CachePackageId (PackageIdentifier pkgname pkgver) blockno)
        _ -> Nothing
    [key, typecodestr, blocknostr] | key == BSS.pack buildTreeRefKey ->
      case (parseRefType typecodestr, parseBlockNo blocknostr) of
        (Just refType, Just blockno)
          -> Just (CacheBuildTreeRef refType blockno)
        _ -> Nothing

    (key: remainder) | key == BSS.pack preferredVersionKey ->
      fmap CachePreference (simpleParse (BSS.unpack (BSS.unwords remainder)))
    _  -> Nothing
  where
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
        Just (blockno, remainder)
          | BSS.null remainder -> Just (fromIntegral blockno)
        _                      -> Nothing

    parseRefType str =
      case BSS.uncons str of
        Just (typeCode, remainder)
          | BSS.null remainder && Tar.isBuildTreeRefTypeCode typeCode
            -> Just (refTypeFromTypeCode typeCode)
        _   -> Nothing

showIndexCacheEntry :: IndexCacheEntry -> String
showIndexCacheEntry entry = unwords $ case entry of
   CachePackageId pkgid b -> [ installedUnitId
                             , display (packageName pkgid)
                             , display (packageVersion pkgid)
                             , blocknoKey
                             , show b
                             ]
   CacheBuildTreeRef t b  -> [ buildTreeRefKey
                             , [typeCodeFromRefType t]
                             , show b
                             ]
   CachePreference dep    -> [ preferredVersionKey
                             , display dep
                             ]

-- | Cabal caches various information about the Hackage index
data Cache = Cache {
    cacheEntries :: [IndexCacheEntry]
  }

readIndexCache :: BSS.ByteString -> Cache
readIndexCache bs = Cache {
    cacheEntries = mapMaybe readIndexCacheEntry $ BSS.lines bs
  }

showIndexCache :: Cache -> String
showIndexCache Cache{..} = unlines $ map showIndexCacheEntry cacheEntries

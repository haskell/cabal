{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
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
  getSourcePackages,
  getSourcePackagesStrict,

  Index(..),
  parsePackageIndex,
  readRepoIndex,
  updateRepoIndexCache,
  updatePackageIndexCacheFile,

  BuildTreeRefType(..), refTypeFromTypeCode, typeCodeFromRefType
  ) where

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
         ( getInstalledPackages )
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

import Data.Char   (isAlphaNum)
import Data.Maybe  (mapMaybe, catMaybes)
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
import System.FilePath ((</>), takeExtension, splitDirectories, normalise)
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error (isDoesNotExistError)


getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramConfiguration
                     -> IO InstalledPackageIndex
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
getSourcePackages verbosity repos = getSourcePackages' verbosity repos
                                    ReadPackageIndexLazyIO

-- | Like 'getSourcePackages', but reads the package index strictly. Useful if
-- you want to write to the package index after having read it.
getSourcePackagesStrict :: Verbosity -> [Repo] -> IO SourcePackageDb
getSourcePackagesStrict verbosity repos = getSourcePackages' verbosity repos
                                          ReadPackageIndexStrict

-- | Common implementation used by getSourcePackages and
-- getSourcePackagesStrict.
getSourcePackages' :: Verbosity -> [Repo] -> ReadPackageIndexMode
                      -> IO SourcePackageDb
getSourcePackages' verbosity [] _mode = do
  warn verbosity $ "No remote package servers have been specified. Usually "
                ++ "you would have one specified in the config file."
  return SourcePackageDb {
    packageIndex       = mempty,
    packagePreferences = mempty
  }
getSourcePackages' verbosity repos mode = do
  info verbosity "Reading available packages..."
  pkgss <- mapM (\r -> readRepoIndex verbosity r mode) repos
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
readRepoIndex :: Verbosity -> Repo -> ReadPackageIndexMode
              -> IO (PackageIndex SourcePackage, [Dependency])
readRepoIndex verbosity repo mode =
  handleNotFound $ do
    warnIfIndexIsOld =<< getIndexFileAge repo
    updateRepoIndexCache verbosity (GlobalIndex repo)
    readPackageIndexCacheFile mkAvailablePackage (GlobalIndex repo) mode

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
    warnIfIndexIsOld dt = do
      when (dt >= isOldThreshold) $ case repoKind repo of
        Left  remoteRepo -> warn verbosity $
             "The package list for '" ++ remoteRepoName remoteRepo
          ++ "' is " ++ shows (floor dt :: Int) " days old.\nRun "
          ++ "'cabal update' to get the latest list of available packages."
        Right _localRepo -> return ()


-- | Return the age of the index file in days (as a Double).
getIndexFileAge :: Repo -> IO Double
getIndexFileAge repo = getFileAge $ repoLocalDir repo </> "00-index.tar"


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

type MkPackageEntry = IO (Maybe PackageEntry)

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

parsePackageIndex :: ByteString
                  -> [IO (Maybe PackageOrDep)]
parsePackageIndex = accum 0 . Tar.read
  where
    accum blockNo es = case es of
      Tar.Fail err   -> error ("parsePackageIndex: " ++ err)
      Tar.Done       -> []
      Tar.Next e es' -> ps ++ accum blockNo' es'
        where
          ps       = extract blockNo e
          blockNo' = blockNo + Tar.entrySizeInBlocks e

    extract blockNo entry = tryExtractPkg ++ tryExtractPrefs
      where
        maybeToList Nothing  = []
        maybeToList (Just a) = [a]

        tryExtractPkg = do
          mkPkgEntry <- maybeToList $ extractPkg entry blockNo
          return $ fmap (fmap Pkg) mkPkgEntry

        tryExtractPrefs = do
          (_,prefs') <- maybeToList $ extractPrefs entry
          fmap (return . Just . Dep) prefs'

extractPkg :: Tar.Entry -> BlockNo -> Maybe MkPackageEntry
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

extractPrefs :: Tar.Entry -> Maybe (FilePath, [Dependency])
extractPrefs entry = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeFileName entrypath == "preferred-versions"
    -> Just (entrypath, prefs)
    where
      entrypath = Tar.entryPath entry
      prefs     = parsePreferredVersions (BS.Char8.unpack content)
  _ -> Nothing

parsePreferredVersions :: String -> [Dependency]
parsePreferredVersions = mapMaybe simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines

------------------------------------------------------------------------
-- Reading and updating the index cache
--

lazySequence :: [IO a] -> IO [a]
lazySequence [] = return []
lazySequence (x:xs) = unsafeInterleaveIO $ do
    x'  <- unsafeInterleaveIO x
    xs' <- lazySequence xs
    return (x':xs')

-- | Which index do we mean?
data Index =
    -- | The global index for the specified repository
    GlobalIndex Repo

    -- | A (sandbox) local repository
  | LocalIndex { localIndexFile :: FilePath, localCacheFile :: FilePath }

indexFile :: Index -> FilePath
indexFile (GlobalIndex repo) = repoLocalDir repo </> "00-index.tar"
indexFile (LocalIndex{..})   = localIndexFile

cacheFile :: Index -> FilePath
cacheFile (GlobalIndex repo) = repoLocalDir repo </> "00-index.cache"
cacheFile (LocalIndex{..})   = localCacheFile

updatePackageIndexCacheFile :: Verbosity -> Index -> IO ()
updatePackageIndexCacheFile verbosity index = do
    info verbosity ("Updating index cache file " ++ cacheFile index)
    pkgsOrPrefs <- return
                 . parsePackageIndex
                 . maybeDecompress
               =<< BS.readFile (indexFile index)
    entries <- lazySequence pkgsOrPrefs
    let cache = Cache { cacheEntries = map toCache $ catMaybes entries }
    writeFile (cacheFile index) (showIndexCache cache)
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
                          -> ReadPackageIndexMode
                          -> IO (PackageIndex pkg, [Dependency])
readPackageIndexCacheFile mkPkg index mode = do
  cache    <- liftM readIndexCache $ BSS.readFile (cacheFile index)
  myWithFile (indexFile index) ReadMode $ \indexHnd ->
    packageIndexFromCache mkPkg indexHnd cache mode
  where
    myWithFile f m act = case mode of
      ReadPackageIndexStrict -> withFile f m act
      ReadPackageIndexLazyIO -> do indexHnd <- openFile f m
                                   act indexHnd


packageIndexFromCache :: Package pkg
                      => (PackageEntry -> pkg)
                      -> Handle
                      -> Cache
                      -> ReadPackageIndexMode
                      -> IO (PackageIndex pkg, [Dependency])
packageIndexFromCache mkPkg hnd Cache{..} mode = accum mempty [] cacheEntries
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
      hSeek hnd AbsoluteSeek (fromIntegral (blockno * 512))
      header  <- BS.hGet hnd 512
      size    <- getEntrySize header
      BS.hGet hnd (fromIntegral size)

    getEntrySize :: ByteString -> IO Tar.FileSize
    getEntrySize header =
      case Tar.read header of
        Tar.Next e _ ->
          case Tar.entryContent e of
            Tar.NormalFile _ size -> return size
            Tar.OtherEntryType typecode _ size
              | Tar.isBuildTreeRefTypeCode typecode
                                  -> return size
            _                     -> interror "unexpected tar entry type"
        _ -> interror "could not read tar file entry"

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
type BlockNo = Int

data IndexCacheEntry = CachePackageId PackageId BlockNo
                     | CacheBuildTreeRef BuildTreeRefType BlockNo
                     | CachePreference Dependency
  deriving (Eq)

installedComponentId, blocknoKey, buildTreeRefKey, preferredVersionKey :: String
installedComponentId = "pkg:"
blocknoKey = "b#"
buildTreeRefKey     = "build-tree-ref:"
preferredVersionKey = "pref-ver:"

readIndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
readIndexCacheEntry = \line ->
  case BSS.words line of
    [key, pkgnamestr, pkgverstr, sep, blocknostr]
      | key == BSS.pack installedComponentId && sep == BSS.pack blocknoKey ->
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
        Just (blockno, remainder) | BSS.null remainder -> Just blockno
        _                                              -> Nothing

    parseRefType str =
      case BSS.uncons str of
        Just (typeCode, remainder)
          | BSS.null remainder && Tar.isBuildTreeRefTypeCode typeCode
            -> Just (refTypeFromTypeCode typeCode)
        _   -> Nothing

showIndexCacheEntry :: IndexCacheEntry -> String
showIndexCacheEntry entry = unwords $ case entry of
   CachePackageId pkgid b -> [ installedComponentId
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

-- | A cache which tracks a value whose validity depends upon
-- the state of various files in the filesystem.

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Distribution.Client.FileStatusCache where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS
import           Data.Binary
import qualified Data.Binary as Binary
import           Data.Foldable
import qualified Data.Hashable as Hashable
import           Data.List (sort)
import           Data.Time (UTCTime(..), Day(..))
import           Data.Monoid ((<>))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Class
import           Control.Exception

import           Distribution.Text
import           Distribution.Compat.ReadP (satisfy, char, (+++))
import qualified Distribution.Compat.ReadP as ReadP
import           Distribution.Client.Glob
import           Distribution.Client.Utils (mergeBy, MergeResult(..))

import           System.FilePath
import           System.Directory
import           System.IO
import           System.IO.Error
import           GHC.Generics (Generic)

type Hash = Int
type ModTime = UTCTime

-- | A path specified by file globbing
data GlobPath = GlobDir Glob GlobPath
              | GlobFile Glob
              deriving (Show, Generic)

instance Binary GlobPath

instance Text GlobPath where
  disp = error "GlobPath disp"
  parse = dir +++ file
    where
      dir = do
        dir <- ReadP.manyTill ReadP.get (satisfy (/= '\\') >> char '/')
        case filter (null . snd) $ ReadP.readP_to_S parse dir of
          [] -> fail "couldn't parse directory glob"
          (x,_):_ -> do void $ char '/'
                        rest <- parse
                        return $ GlobDir x rest
      file = GlobFile <$> parse

-- | A list of search paths
type SearchPaths = [FilePath]

-- | A description of the the files our cache is interested in.
--
-- File paths here are relative to the root directory.
data FileSpec = SingleHashedFile FilePath
              | SingleFile FilePath
              | GlobHashPath GlobPath
                -- ^ Look for a file matching a glob pattern, checking
                -- its hash for changes if necessary.
              | SearchPath SearchPaths FilePath
                -- ^ Find the first occurrence of the file with the given
                -- file name in the given list of directories
              deriving (Show)

-- | invariant: the lists of pairs above are sorted
--
-- This is where we preserve the state necessary to determine whether
-- the files matched by a globbing match has changed.
data CachedGlobPath = CSubdirs Glob GlobPath ModTime [(FilePath, CachedGlobPath)]
                    | CFiles Glob ModTime [(FilePath, ModTime, Hash)]
                    deriving (Show, Generic, Binary)

-- | @CSearchPath paths_searched file_name found_dir mod_time@
data CachedSearchPath = CSearchPath [FilePath] String (Maybe (FilePath, ModTime))
                      deriving (Show, Generic)
instance Binary CachedSearchPath

-- | The cached status of a file.
--
-- All file paths are relative to the root directory.
data CachedFileSpec = CSingleHashedFile ModTime Hash
                    | CSingleFile ModTime
                    | CSingleFileNotFound
                    deriving (Show, Generic)
instance Binary CachedFileSpec

data FileStatusCache = FileStatusCache (Map FilePath CachedFileSpec)
                                       [CachedGlobPath]
                                       [CachedSearchPath]
                     deriving (Show)

instance Monoid FileStatusCache where
  mempty = FileStatusCache Map.empty [] []
  FileStatusCache a b c `mappend` FileStatusCache x y z =
    FileStatusCache (a<>x) (b<>y) (c<>z)

-- | Does a 'CachedGlobPath' have any relevant files within it?
hasMatchingFiles :: CachedGlobPath -> Bool
hasMatchingFiles (CFiles _ _ files) = not $ null files
hasMatchingFiles (CSubdirs _ _ _ children) = any (hasMatchingFiles . snd) children

data CacheValidity = CacheChanged | CacheUnchanged

allCacheUnchanged :: [CacheValidity] -> CacheValidity
allCacheUnchanged xs
  | null [() | CacheChanged <- xs] = CacheUnchanged
  | otherwise                      = CacheChanged

type ChangedT = State.StateT CacheValidity (MaybeT IO)

somethingChanged :: ChangedT a
somethingChanged = lift $ MaybeT $ return Nothing

cacheChanged :: ChangedT ()
cacheChanged = State.put CacheChanged

-- |
-- We may need to update the cache since there may be changes in the filesystem
-- state which don't change any of our affected files. Consider the glob
-- @{proj1,proj2}/*.cabal@. Say we first run and find a @proj1@ directory
-- containing @proj1.cabal@ yet no @proj2@. If we later run and find @proj2@ was
-- created, yet contains no files matching @*.cabal@ then we want to update the
-- cache despite no changes in our relevant file set. Specifically, we should
-- add an mtime for this directory so we can avoid re-traversing the directory
-- in future runs.
checkFileStatusChanged :: (Binary a)
                       => FilePath       -- ^ cache file path
                       -> FilePath       -- ^ root directory
                       -> IO (Changed a) -- ^ did any of these paths change?
checkFileStatusChanged statusCacheFile root = do
    mfileCache <- handleDoesNotExist (\_ -> return Nothing) $
                    either (\_ -> return Nothing)
                           (return . Just)
                       =<< Binary.decodeFileOrFail statusCacheFile
    case mfileCache of
      Nothing -> return Changed
      Just (FileStatusCache singlePaths globPaths searchPaths , cachedValue) -> do
         res <- runMaybeT $ flip State.runStateT CacheUnchanged $ do
           singlePaths' <- Map.traverseWithKey probeSingle singlePaths
           globPaths' <- traverse (probeGlobPath ".") globPaths
           searchPaths' <- traverse probeSearch searchPaths
           return $ FileStatusCache singlePaths' globPaths' searchPaths'
         case res of
           Nothing -> return Changed
           Just (cache', CacheChanged) -> do
             Binary.encodeFile statusCacheFile (cache', cachedValue)
             return (Unchanged cachedValue)
           Just (_, CacheUnchanged) -> return (Unchanged cachedValue)
  where
    -- | Returns whether the returned cache matches the previous cache
    probeSingle :: FilePath -> CachedFileSpec -> ChangedT CachedFileSpec
    probeSingle file CSingleFileNotFound = do
      exists <- liftIO $ doesFileExist (root </> file)
      if exists
        then somethingChanged
        else return CSingleFileNotFound

    probeSingle file cached@(CSingleFile mtime) = do
      same <- liftIO $ probeModificationTime (root </> file) mtime
      unless same somethingChanged
      return cached

    probeSingle file cached@(CSingleHashedFile mtime hash) = do
      probeHashedFile (root </> file) mtime hash
      return cached

    probeSearch :: CachedSearchPath -> ChangedT CachedSearchPath
    probeSearch cached@(CSearchPath searchDirs name found) = do
      -- First make sure it isn't in any of the early search directories
      file' <- liftIO $ findFile (map (root</>) searchDirs) name
      case file' of
        Just _ -> somethingChanged
        Nothing
          | Just (foundDir, mtime) <- found -> do
            same <- liftIO $ probeModificationTime (root </> foundDir </> name) mtime
            unless same somethingChanged
          | otherwise                       -> return ()
      return cached

    probeGlobPath :: FilePath      -- ^ path of the directory we are looking in relative to @root@
                  -> CachedGlobPath
                  -> ChangedT CachedGlobPath
                     -- ^ returns True if previous CachedGlobPath is still valid
    probeGlobPath dirName (CSubdirs glob globPath mtime children) = do
        same <- liftIO $ probeModificationTime (root </> dirName) mtime

        res <- if same
                 then sequence [ do cgp' <- probeGlobPath (dirName </> fname) cgp
                                    return (fname, cgp')
                               | (fname, cgp) <- children ]
                 else do allEnts <- liftIO $ getDirectoryContents (root </> dirName)
                         ents <- filterM (liftIO . doesDirectoryExist)
                                 $ filter (globMatches glob . normalise) allEnts
                         mapM probeMergeResult
                              $ mergeBy (\(path1,_) path2 -> compare path1 path2)
                                        children
                                        (sort ents)
        return $ CSubdirs glob globPath mtime res
      where
        probeMergeResult :: MergeResult (FilePath, CachedGlobPath) FilePath
                         -> ChangedT (FilePath, CachedGlobPath)
                            -- ^ Return True if previous CachedGlobPath is still valid
        -- Only in cached (directory deleted)
        probeMergeResult (OnlyInLeft (path, cgp))
          | not (hasMatchingFiles cgp) = return (path, cgp)
            -- Strictly speaking we should be returning 'CacheChanged' above
            -- as we should prune the now-missing 'CachedGlobPath'. However
            -- we currently just leave these now-redundant entries in the
            -- cache as they cost no IO and keeping them allows us to avoid
            -- rewriting the cache.
          | otherwise                  = somethingChanged

        -- Only in current filesystem state (directory added)
        probeMergeResult (OnlyInRight path) = do
          cgp <- liftIO $ buildCachedGlobPath root (dirName </> path) globPath
          if hasMatchingFiles cgp
            then somethingChanged
            else do cacheChanged
                    return (path, cgp)

        -- Found in path
        probeMergeResult (InBoth (path, cgp) _) = do
          cgp' <- probeGlobPath (dirName </> path) cgp
          return (dirName </> path, cgp')

    probeGlobPath dirName cached@(CFiles glob mtime children) = do
        same <- liftIO $ probeModificationTime (root </> dirName) mtime
        unless same $ do
            -- Modification time changed:
            -- a file may have been added or deleted
            allEnts <- liftIO $ getDirectoryContents (root </> dirName)
            ents <- filterM (\fname -> liftIO $ doesFileExist $ root </> dirName </> fname)
                    $ filter (globMatches glob . normalise) allEnts
            let mergeRes = mergeBy (\(path1,_,_) path2 -> compare path1 path2)
                            children
                            (sort ents)
            unless (all isInBoth mergeRes) somethingChanged

        -- Check that none of the children have changed
        forM_ children $ \(file, mtime', hash) ->
            probeHashedFile (root </> dirName </> file) mtime' hash
        return cached -- FIXME?
      where
        isInBoth :: MergeResult a b -> Bool
        isInBoth (InBoth _ _) = True
        isInBoth _            = False

    probeHashedFile :: FilePath -> ModTime -> Hash -> ChangedT ()
    probeHashedFile file mtime hash = do
      sameMTime <- liftIO $ probeModificationTime file mtime
      unless sameMTime $ do
           sameHash <- liftIO $ probeFileHash file hash
           unless sameHash somethingChanged

    probeModificationTime :: FilePath -> ModTime -> IO Bool
    probeModificationTime file mtime =
      handleDoesNotExist (\_ -> return False) $ do
       mtime' <- getModificationTime file
       return (mtime == mtime')

    probeFileHash file chash =
      handleDoesNotExist (\_ -> return False) $ do
        chash' <- readFileHash file

        --TODO: debug only:
        when (chash == chash') $
          print ("checkFileStatusChanged", file, chash, chash')

        return (chash == chash')

buildCachedGlobPath :: FilePath -- ^ the root directory
                    -> FilePath -- ^ directory we are examining relative to the root
                    -> GlobPath -- ^ the matching glob
                    -> IO CachedGlobPath
buildCachedGlobPath root dir globPath = do
    ents <- getDirectoryContents (root </> dir)
    dirMTime <- getModificationTime (root </> dir)
    case globPath of
      GlobDir glob globPath' -> do
        all' <- filterM doesDirectoryExist $ filter (globMatches glob) ents
        subdirs <- forM all' $ \subdir -> do
          cgp <- buildCachedGlobPath root (dir </> subdir) globPath'
          return (subdir, cgp)
        return $ CSubdirs glob globPath' dirMTime subdirs

      GlobFile glob -> do
        ents' <- filterM (\fname -> doesFileExist (root </> dir </> fname))
                 $ filter (globMatches glob) ents
        files <- forM ents' $ \file -> do
          let path = root </> dir </> file
          mtime <- getModificationTime path
          hash <- readFileHash path
          return (file, mtime, hash)
        return $ CFiles glob dirMTime files

updateFileStatusCache
    :: (Binary a)
    => FilePath   -- ^ cache file path
    -> FilePath   -- ^ root directory
    -> [FileSpec] -- ^ patterns of interest relative to root
    -> a          -- ^ a cached value dependent upon the paths identified by
                  -- the given patterns
    -> IO ()
updateFileStatusCache cacheFile root specs cachedValue = do
    fsc <- genFileStatusCache root specs
    Binary.encodeFile cacheFile (fsc, cachedValue)

genFileStatusCache
    :: FilePath   -- ^ root directory
    -> [FileSpec] -- ^ patterns of interest relative to root
    -> IO FileStatusCache
genFileStatusCache root specs = do
    fold <$> mapM go specs
  where
    go :: FileSpec -> IO FileStatusCache
    go (SingleHashedFile path) = do
      let file = root </> path
      exists <- doesFileExist file
      cfs <- if exists
               then do fileHash <- readFileHash file
                       mtime <- getModificationTime file
                       return $ CSingleHashedFile mtime fileHash
               else return CSingleFileNotFound
      return $ FileStatusCache (Map.singleton path cfs) [] []

    go (SingleFile path) = do
      let file = root </> path
      exists <- doesFileExist file
      cfs <- if exists
               then do mtime <- getModificationTime file
                       return $ CSingleFile mtime
               else return CSingleFileNotFound
      return $ FileStatusCache (Map.singleton path cfs) [] []

    go (GlobHashPath globPath) = do
      cgp <- buildCachedGlobPath root "." globPath
      return $ FileStatusCache mempty [cgp] []

    go (SearchPath paths0 name) = do
        csp <- search [] paths0
        return $ FileStatusCache mempty [] [csp]
      where
        search :: [FilePath] -> [FilePath] -> IO CachedSearchPath
        search searched [] = return $ CSearchPath searched name Nothing
        search searched (dir:paths) = do
          let path = root </> dir </> name
          exists <- doesFileExist path
          if exists
            then do mtime <- getModificationTime path
                    return $ CSearchPath searched name $ Just (dir, mtime)
            else search (dir:searched) paths

readFileHash :: FilePath -> IO Hash
readFileHash file =
    withBinaryFile file ReadMode $ \hnd ->
      evaluate . Hashable.hash =<< BS.hGetContents hnd

{-
readFileInfo :: FilePath -> IO FileInfo
readFileInfo file =
    FileInfo <$> getModificationTime file
             <*> readFileHash file
-}

handleDoesNotExist :: (IOError -> IO a) -> IO a -> IO a
handleDoesNotExist =
    handleJust (\ioe -> if isDoesNotExistError ioe then Just ioe else Nothing)

{-
instance Binary FileInfo where
  put (FileInfo a b) = do put a >> put b
  get = do a <- get; b <- get; return $! FileInfo a b-}
{--}

instance Binary UTCTime where
  put (UTCTime (ModifiedJulianDay day) tod) = do
    put day
    put (toRational tod)
  get = do
    day  <- get
    tod <- get
    return $! UTCTime (ModifiedJulianDay day)
                      (fromRational tod)

instance Binary FileStatusCache where
  put (FileStatusCache singlePaths globPaths searchPaths) = do
    put (1 :: Int) -- version
    put singlePaths
    put globPaths
    put searchPaths
  get = do
    ver <- get
    if ver == (1 :: Int)
      then do singlePaths <- get
              globPaths <- get
              searchPaths <- get
              return $! FileStatusCache singlePaths globPaths searchPaths
      else fail "FileStatusCache: wrong version"

---------------------------------------------------------------------

data Changed b = Changed | Unchanged b
  deriving Show

checkValueChanged :: (Binary a, Eq a, Binary b)
                  => FilePath -> a -> IO (Changed b)
checkValueChanged cacheFile currentValue =
    handleDoesNotExist (\_ -> return Changed) $ do   -- cache file didn't exist
      res <- Binary.decodeFileOrFail cacheFile
      case res of
        Right (cachedValue, cachedPayload)
          | currentValue == cachedValue
                       -> return (Unchanged cachedPayload)
          | otherwise  -> return Changed -- value changed
        Left _         -> return Changed -- decode error


updateValueChangeCache :: (Binary a, Binary b) => FilePath -> a -> b -> IO ()
updateValueChangeCache path key payload = Binary.encodeFile path (key, payload)

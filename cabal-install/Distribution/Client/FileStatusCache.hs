-- | A cache which tracks a value whose validity depends upon
-- the state of various files in the filesystem.

{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, BangPatterns #-}

module Distribution.Client.FileStatusCache (
  
  -- * Declaring files to monitor
  MonitorFilePath(..),
  FilePathGlob(..),
  monitorFileSearchPath,
  monitorFileHashedSearchPath,

  -- * Creating and checking sets of monitored files
  FileMonitorCacheFile(..),
  Changed(..),
  checkFileMonitorChanged,
  updateFileMonitor,

  matchFileGlob,
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS
import           Data.Binary
import qualified Data.Binary as Binary
import           Data.Traversable (traverse)
import qualified Data.Hashable as Hashable
import           Data.List (sort)
import           Data.Time (UTCTime(..), Day(..))

import           Control.Monad
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as State
import           Control.Monad.Except
import           Control.Exception

import           Distribution.Text
import           Distribution.Compat.ReadP ((<++))
import qualified Distribution.Compat.ReadP as ReadP
import qualified Text.PrettyPrint as Disp

import           Distribution.Client.Glob
import           Distribution.Client.Utils (mergeBy, MergeResult(..))

import           System.FilePath
import           System.Directory
import           System.IO
import           System.IO.Error
import           GHC.Generics (Generic)


------------------------------------------------------------------------------
-- Types for specifying files to monitor
--


-- | A description of a file (or set of files) to monitor for changes.
--
-- All file paths here are relative to a common directory (e.g. project root).
--
data MonitorFilePath =

     -- | Monitor a single file for changes, based on its modification time.
     -- The monitored file is considered to have changed if it no longer
     -- exists or if its modification time has changed.
     --
     MonitorFile !FilePath

     -- | Monitor a single file for changes, based on its modification time
     -- and content hash. The monitored file is considered to have changed if
     -- it no longer exists or if its modification time and content hash have
     -- changed.
     --
   | MonitorFileHashed !FilePath

     -- | Monitor a single non-existent file for changes. The monitored file
     -- is considered to have changed if it exists.
     --
   | MonitorNonExistentFile !FilePath

     -- | Monitor a set of files identified by a file glob. The monitored glob
     -- is considered to have changed if the set of files matching the glob
     -- changes (i.e. creations or deletions), or if the modification time and
     -- content hash of any matching file has changed.
     --
   | MonitorFileGlob !FilePathGlob

  deriving (Show, Generic)

instance Binary MonitorFilePath

-- | A file path specified by globbing
--
data FilePathGlob
   = GlobDir  !Glob !FilePathGlob
   | GlobFile !Glob
  deriving (Eq, Show, Generic)

instance Binary FilePathGlob

-- | Creates a list of files to monitor when you search for a file which
-- unsuccessfully looked in @notFoundAtPaths@ before finding it at
-- @foundAtPath@.
monitorFileSearchPath :: [FilePath] -> FilePath -> [MonitorFilePath]
monitorFileSearchPath notFoundAtPaths foundAtPath =
    MonitorFile foundAtPath
  : map MonitorNonExistentFile notFoundAtPaths

-- | Similar to 'monitorFileSearchPath', but also instructs us to
-- monitor the hash of the found file.
monitorFileHashedSearchPath :: [FilePath] -> FilePath -> [MonitorFilePath]
monitorFileHashedSearchPath notFoundAtPaths foundAtPath =
    MonitorFileHashed foundAtPath
  : map MonitorNonExistentFile notFoundAtPaths

-- | A file used to store the state of a file monitor.
--
-- It is typed to ensure it's used at consistent types.
--
newtype FileMonitorCacheFile a b
      = FileMonitorCacheFile { monitorCacheFilePath :: FilePath }
  deriving Show

------------------------------------------------------------------------------
-- Implementation types, files status
--

-- | The state necessary to determine whether a set of monitored
-- files has changed.  It consists of two parts: a set of specific
-- files to be monitored (index by their path), and a list of
-- globs, which monitor may files at once.
data MonitorStateFileSet
   = MonitorStateFileSet !(Map FilePath MonitorStateFile)
                         ![FileGlobMonitorState]
  deriving Show

--instance Monoid MonitorStateFileSet where
--  mempty = MonitorStateFileSet Map.empty []
--  MonitorStateFileSet a b `mappend` MonitorStateFileSet x y =
--    MonitorStateFileSet (a<>x) (b<>y)

type Hash = Int
type ModTime = UTCTime

-- | The state necessary to determine whether a monitored file has changed.
--
-- This covers all the cases of 'MonitorFilePath' except for globs which is
-- covered separately by 'FileGlobMonitorState'.
--
data MonitorStateFile
   = MonitorStateFile       !ModTime         -- ^ cached file mtime
   | MonitorStateFileHashed !ModTime !Hash   -- ^ cached file mtime and content hash
   | MonitorStateFileNonExistent

     -- | These two are to deal with the situation where we've been asked
     -- to monitor a file that's expected to exist, but when we come to
     -- check it's status, it no longer exists.
   | MonitorStateFileChanged
   | MonitorStateFileHashChanged
  deriving (Show, Generic)

instance Binary MonitorStateFile

-- | The state necessary to determine whether the files matched by a globbing
-- match have changed.
--
data FileGlobMonitorState
   = MonitorStateGlobDirs  !Glob !FilePathGlob
                           !ModTime
                           ![(FilePath, FileGlobMonitorState)] -- invariant: sorted

   | MonitorStateGlobFiles !Glob
                           !ModTime
                           ![(FilePath, ModTime, Hash)] -- invariant: sorted
  deriving (Show, Generic)

instance Binary FileGlobMonitorState

-- We can build a 'MonitorStateFileSet' from a set of 'MonitorFilePath' by
-- inspecting the state of the file system, and we can go in the reverse
-- direction by just forgetting the extra info.
--
reconstructMonitorFilePaths :: MonitorStateFileSet -> [MonitorFilePath]
reconstructMonitorFilePaths (MonitorStateFileSet singlePaths globPaths) =
    Map.foldrWithKey (\k x r -> getSinglePath k x : r)
                     (map getGlobPath globPaths)
                     singlePaths
  where
    getSinglePath filepath monitorState =
      case monitorState of
        MonitorStateFile{}          -> MonitorFile            filepath
        MonitorStateFileHashed{}    -> MonitorFileHashed      filepath
        MonitorStateFileNonExistent -> MonitorNonExistentFile filepath
        MonitorStateFileChanged     -> MonitorFile            filepath
        MonitorStateFileHashChanged -> MonitorFileHashed      filepath

    getGlobPath (MonitorStateGlobDirs  glob globs _ _) =
      MonitorFileGlob (GlobDir  glob globs)
    getGlobPath (MonitorStateGlobFiles glob       _ _) =
      MonitorFileGlob (GlobFile glob)

------------------------------------------------------------------------------
-- Checking the status of monitored files
--

-- | Encapsulates a value that is either unchanged, or needs
-- to be recomputed.
data Changed b =
    -- | The value is unchanged, and it is @b@.
      Unchanged b
    -- | The value is changed.  @Just f@ indicates @f@ was the
    -- file which changed which required a rebuild; @Nothing@
    -- indicates the value changed for some other reason.
    | Changed (Maybe FilePath)
  deriving Show


-- | Attempt to lookup @currentKey@ from the 'FileMonitorCacheFile'
-- for some @root@ directory.  This returns @Unchanged@ if the
-- value is fresh (with the files which are monitored), and @Changed@ if
-- something changed and this needs to be recomputed.  See also the
-- 'Rebuild' monad, which uses this check and rebuilds the value if it
-- is stale.
checkFileMonitorChanged
  :: (Eq a, Binary a, Binary b)
  => FileMonitorCacheFile a b            -- ^ cache file path
  -> FilePath                            -- ^ root directory
  -> a                                   -- ^ key value
  -> IO (Changed (b, [MonitorFilePath])) -- ^ did the key or any paths change?
checkFileMonitorChanged (FileMonitorCacheFile monitorStateFile)
                        root currentKey =

    -- Consider it a change if the cache file does not exist,
    -- or we cannot decode it. Sadly ErrorCall can still happen, despite
    -- using decodeFileOrFail, e.g. Data.Char.chr errors

    handleDoesNotExist (Changed Nothing) $
    handleErrorCall    (Changed Nothing) $
          Binary.decodeFileOrFail monitorStateFile
      >>= either (\_ -> return (Changed Nothing))
                 checkStatusCache

  where
    -- It's also a change if the guard value has changed
    checkStatusCache (_, cachedKey, _)
      | currentKey /= cachedKey
      = return (Changed Nothing)

    checkStatusCache (cachedFileStatus, cachedKey, cachedResult) = do

      -- Now we have to go probe the file system.
      res <- probeFileSystem root cachedFileStatus
      case res of
        -- Some monitored file has changed
        Left changedPath -> return (Changed (Just changedPath))

        -- No monitored file has changed
        Right (cachedFileStatus', cacheStatus) -> do

          -- But we might still want to update the cache
          whenCacheChanged cacheStatus $
            rewriteCache cachedFileStatus' cachedKey cachedResult

          let monitorFiles = reconstructMonitorFilePaths cachedFileStatus'
          return (Unchanged (cachedResult, monitorFiles))

    rewriteCache cachedFileStatus' cachedKey cachedResult = 
      Binary.encodeFile monitorStateFile
                        (cachedFileStatus', cachedKey, cachedResult)

-- | Probe the file system to see if any of the monitored files have changed.
--
-- It returns Nothing if any file changed, or returns a possibly updated
-- file 'MonitorStateFileSet' plus an indicator of whether it actually changed.
--
-- We may need to update the cache since there may be changes in the filesystem
-- state which don't change any of our affected files.
--
-- Consider the glob @{proj1,proj2}/*.cabal@. Say we first run and find a
-- @proj1@ directory containing @proj1.cabal@ yet no @proj2@. If we later run
-- and find @proj2@ was created, yet contains no files matching @*.cabal@ then
-- we want to update the cache despite no changes in our relevant file set.
-- Specifically, we should add an mtime for this directory so we can avoid
-- re-traversing the directory in future runs.
--
probeFileSystem :: FilePath -> MonitorStateFileSet
                -> IO (Either FilePath (MonitorStateFileSet, CacheChanged))
probeFileSystem root (MonitorStateFileSet singlePaths globPaths) =
  runChangedM $
    MonitorStateFileSet
      <$> Map.traverseWithKey (probeFileStatus root)     singlePaths
      <*> traverse            (probeGlobStatus root ".") globPaths

-----------------------------------------------
-- Monad for checking for file system changes
--
-- We need to be able to bail out if we detect a change (using ExceptT),
-- but if there's no change we need to be able to rebuild the monitor
-- state. And we want to optimise that rebuilding by keeping track if
-- anything actually changed (using StateT), so that in the typical case
-- we can avoid rewriting the state file.

newtype ChangedM a = ChangedM (StateT CacheChanged (ExceptT FilePath IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

runChangedM :: ChangedM a -> IO (Either FilePath (a, CacheChanged))
runChangedM (ChangedM action) =
  runExceptT $ State.runStateT action CacheUnchanged

somethingChanged :: FilePath -> ChangedM a
somethingChanged path = ChangedM $ throwError path

cacheChanged :: ChangedM ()
cacheChanged = ChangedM $ State.put CacheChanged

data CacheChanged = CacheChanged | CacheUnchanged

whenCacheChanged :: Monad m => CacheChanged -> m () -> m ()
whenCacheChanged CacheChanged action = action
whenCacheChanged CacheUnchanged _    = return ()

----------------------

-- | Probe the file system to see if a single monitored file has changed.
--
probeFileStatus :: FilePath -> FilePath -> MonitorStateFile
                -> ChangedM MonitorStateFile
probeFileStatus root file cached = do
    case cached of
      MonitorStateFile       mtime      -> probeFileModificationTime
                                             root file mtime
      MonitorStateFileHashed mtime hash -> probeFileModificationTimeAndHash
                                             root file mtime hash
      MonitorStateFileNonExistent       -> probeFileNonExistance root file
      MonitorStateFileChanged           -> somethingChanged file
      MonitorStateFileHashChanged       -> somethingChanged file

    return cached


-- | Probe the file system to see if a monitored file glob has changed.
--
probeGlobStatus :: FilePath      -- ^ root path
                -> FilePath      -- ^ path of the directory we are looking in relative to @root@
                -> FileGlobMonitorState
                -> ChangedM FileGlobMonitorState
probeGlobStatus root dirName (MonitorStateGlobDirs glob globPath mtime children) = do
    change <- liftIO $ checkDirectoryModificationTime (root </> dirName) mtime
    case change of
      Nothing -> do
        children' <- sequence
                       [ do cgp' <- probeGlobStatus root (dirName </> fname) cgp
                            return (fname, cgp')
                       | (fname, cgp) <- children ]
        return $! MonitorStateGlobDirs glob globPath mtime children'

      Just mtime' -> do
        -- directory modification time changed:
        -- a matching subdir may have been added or deleted
        matches <- filterM (\entry -> let subdir = root </> dirName </> entry
                                       in liftIO $ doesDirectoryExist subdir)
                 . filter (globMatches glob)
               =<< liftIO (getDirectoryContents (root </> dirName))

        children' <- mapM probeMergeResult $
                          mergeBy (\(path1,_) path2 -> compare path1 path2)
                                  children
                                  (sort matches)
        return $! MonitorStateGlobDirs glob globPath mtime' children'
        -- Note that just because the directory has changed, we don't force
        -- a cache rewite with 'cacheChanged' since that has some cost, and
        -- all we're saving is scanning the directory. But we do rebuild the
        -- cache with the new mtime', so that if the cache is rewritten for
        -- some other reason, we'll take advantage of that.
    

  where
    probeMergeResult :: MergeResult (FilePath, FileGlobMonitorState) FilePath
                     -> ChangedM (FilePath, FileGlobMonitorState)

    -- Only in cached (directory deleted)
    probeMergeResult (OnlyInLeft (path, cgp))
      | not (hasMatchingFiles cgp) = return (path, cgp)
        -- Strictly speaking we should be returning 'CacheChanged' above
        -- as we should prune the now-missing 'FileGlobMonitorState'. However
        -- we currently just leave these now-redundant entries in the
        -- cache as they cost no IO and keeping them allows us to avoid
        -- rewriting the cache.
      | otherwise = somethingChanged path

    -- Only in current filesystem state (directory added)
    probeMergeResult (OnlyInRight path) = do
      cgp <- liftIO $ buildFileGlobMonitorState root (dirName </> path) globPath
      if hasMatchingFiles cgp
        then somethingChanged path
        else cacheChanged >> return (path, cgp)

    -- Found in path
    probeMergeResult (InBoth (path, cgp) _) = do
      cgp' <- probeGlobStatus root (dirName </> path) cgp
      return (path, cgp')

    -- | Does a 'FileGlobMonitorState' have any relevant files within it?
    hasMatchingFiles :: FileGlobMonitorState -> Bool
    hasMatchingFiles (MonitorStateGlobFiles _ _   entries) = not (null entries)
    hasMatchingFiles (MonitorStateGlobDirs  _ _ _ entries) =
      any (hasMatchingFiles . snd) entries


probeGlobStatus root dirName (MonitorStateGlobFiles glob mtime children) = do
    change <- liftIO $ checkDirectoryModificationTime (root </> dirName) mtime
    mtime' <- case change of
      Nothing     -> return mtime
      Just mtime' -> do
        -- directory modification time changed:
        -- a matching file may have been added or deleted
        matches <- filterM (\entry -> let file = root </> dirName </> entry
                                       in liftIO $ doesFileExist file)
                 . filter (globMatches glob)
               =<< liftIO (getDirectoryContents (root </> dirName))

        let mergeRes = mergeBy (\(path1,_,_) path2 -> compare path1 path2)
                         children
                         (sort matches)
        unless (all isInBoth mergeRes) (somethingChanged dirName)
        return mtime'

    -- Check that none of the children have changed
    forM_ children $ \(file, fmtime, fhash) ->
        probeFileModificationTimeAndHash root (dirName </> file) fmtime fhash

    return (MonitorStateGlobFiles glob mtime' children)
    -- Again, we don't force a cache rewite with 'cacheChanged', but we do use
    -- the new mtime' if any.
  where
    isInBoth :: MergeResult a b -> Bool
    isInBoth (InBoth _ _) = True
    isInBoth _            = False

------------------------------------------------------------------------------

updateFileMonitor
  :: (Binary a, Binary b)
  => FileMonitorCacheFile a b -- ^ cache file path
  -> FilePath                 -- ^ root directory
  -> [MonitorFilePath]        -- ^ patterns of interest relative to root
  -> a                        -- ^ a cached key value
  -> b                        -- ^ a cached value dependent upon the key and on
                              --   the paths identified by the given patterns
  -> IO ()
updateFileMonitor (FileMonitorCacheFile cacheFile) root
                  monitorFiles cachedKey cachedResult = do
    fsc <- buildMonitorStateFileSet root monitorFiles
    Binary.encodeFile cacheFile (fsc, cachedKey, cachedResult)

buildMonitorStateFileSet :: FilePath          -- ^ root directory
                         -> [MonitorFilePath] -- ^ patterns of interest relative to root
                         -> IO MonitorStateFileSet
buildMonitorStateFileSet root =
    go Map.empty []
  where
    go :: Map FilePath MonitorStateFile -> [FileGlobMonitorState]
       -> [MonitorFilePath] -> IO MonitorStateFileSet
    go !singlePaths !globPaths [] =
      return (MonitorStateFileSet singlePaths globPaths)

    go !singlePaths !globPaths (MonitorFile path : monitors) = do
      let file = root </> path
      monitorState <- handleDoesNotExist MonitorStateFileChanged $
                        MonitorStateFile <$> getModificationTime file
      let singlePaths' = Map.insert path monitorState singlePaths
      go singlePaths' globPaths monitors

    go !singlePaths !globPaths (MonitorFileHashed path : monitors) = do
      let file = root </> path
      monitorState <- handleDoesNotExist MonitorStateFileHashChanged $
                        MonitorStateFileHashed
                          <$> getModificationTime file
                          <*> readFileHash file
      let singlePaths' = Map.insert path monitorState singlePaths
      go singlePaths' globPaths monitors

    go !singlePaths !globPaths (MonitorNonExistentFile path : monitors) = do
      let singlePaths' = Map.insert path MonitorStateFileNonExistent singlePaths
      go singlePaths' globPaths monitors

    go !singlePaths !globPaths (MonitorFileGlob globPath : monitors) = do
      monitorState <- buildFileGlobMonitorState root "." globPath
      go singlePaths (monitorState : globPaths) monitors


buildFileGlobMonitorState :: FilePath     -- ^ the root directory
                          -> FilePath     -- ^ directory we are examining relative to the root
                          -> FilePathGlob -- ^ the matching glob
                          -> IO FileGlobMonitorState
buildFileGlobMonitorState root dir globPath = do
    dirEntries <- getDirectoryContents (root </> dir)
    dirMTime   <- getModificationTime (root </> dir)
    case globPath of
      GlobDir glob globPath' -> do
        subdirs <- filterM (\subdir -> doesDirectoryExist (root </> dir </> subdir))
                 $ filter (globMatches glob) dirEntries
        subdirStates <-
          forM subdirs $ \subdir -> do
            cgp <- buildFileGlobMonitorState root (dir </> subdir) globPath'
            return (subdir, cgp)
        return $! MonitorStateGlobDirs glob globPath' dirMTime subdirStates

      GlobFile glob -> do
        files <- filterM (\fname -> doesFileExist (root </> dir </> fname))
               $ filter (globMatches glob) dirEntries
        filesStates <-
          forM (sort files) $ \file -> do
            let path = root </> dir </> file
            mtime <- getModificationTime path
            hash  <- readFileHash path
            return (file, mtime, hash)
        return $! MonitorStateGlobFiles glob dirMTime filesStates

matchFileGlob :: FilePath -> FilePathGlob -> IO [FilePath]
matchFileGlob root glob0 = go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (globMatches glob) entries
      return (map (dir </>) files)

    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <- filterM (\subdir -> doesDirectoryExist (root </> dir </> subdir))
               $ filter (globMatches glob) entries
      concat <$> mapM (\subdir -> go globPath (dir </> subdir)) subdirs
 

------------------------------------------------------------------------------
-- Utils
-- 

-- | Within the @root@ directory, check if @file@ has its 'ModTime' is
-- the same as @mtime@, short-circuiting if it is different.
probeFileModificationTime :: FilePath -> FilePath -> ModTime -> ChangedM ()
probeFileModificationTime root file mtime = do
    unchanged <- liftIO $ checkModificationTimeUnchanged root file mtime
    unless unchanged (somethingChanged file)

-- | Within the @root@ directory, check if @file@ has its 'ModTime' and
-- 'Hash' is the same as @mtime@ and @hash@, short-circuiting if it is
-- different.
probeFileModificationTimeAndHash :: FilePath -> FilePath -> ModTime -> Hash
                                 -> ChangedM ()
probeFileModificationTimeAndHash root file mtime hash = do
    unchanged <- liftIO $
      checkFileModificationTimeAndHashUnchanged root file mtime hash
    unless unchanged (somethingChanged file)

-- | Within the @root@ directory, check if @file@ still does not exist.
-- If it *does* exist, short-circuit.
probeFileNonExistance :: FilePath -> FilePath -> ChangedM ()
probeFileNonExistance root file = do
    exists <- liftIO $ doesFileExist (root </> file)
    when exists (somethingChanged file)

-- | Returns @True@ if, inside the @root@ directory, @file@ has the same
-- 'ModTime' as @mtime@.
checkModificationTimeUnchanged :: FilePath -> FilePath
                               -> ModTime -> IO Bool
checkModificationTimeUnchanged root file mtime =
  handleDoesNotExist False $ do
    mtime' <- getModificationTime (root </> file)
    return (mtime == mtime')

-- | Returns @True@ if, inside the @root@ directory, @file@ has the
-- same 'ModTime' and 'Hash' as @mtime and @chash@.
checkFileModificationTimeAndHashUnchanged :: FilePath -> FilePath
                                          -> ModTime -> Hash -> IO Bool
checkFileModificationTimeAndHashUnchanged root file mtime chash =
  handleDoesNotExist False $ do
    mtime' <- getModificationTime (root </> file)
    
    if mtime == mtime'
      then return True
      else do
        chash' <- readFileHash (root </> file)
        return (chash == chash')

-- | Read a non-cryptographic hash of a @file@.
readFileHash :: FilePath -> IO Hash
readFileHash file =
    withBinaryFile file ReadMode $ \hnd ->
      evaluate . Hashable.hash =<< BS.hGetContents hnd

-- | Given a directory @dir@, return @Nothing@ if its 'ModTime'
-- is the same as @mtime@, and the new 'ModTime' if it is not.
checkDirectoryModificationTime :: FilePath -> ModTime -> IO (Maybe ModTime)
checkDirectoryModificationTime dir mtime =
  handleDoesNotExist Nothing $ do
    mtime' <- getModificationTime dir
    if mtime == mtime'
      then return Nothing
      else return (Just mtime')

-- | Run an IO computation, returning @e@ if it raises a "file
-- does not exist" error.
handleDoesNotExist :: a -> IO a -> IO a
handleDoesNotExist e =
    handleJust
      (\ioe -> if isDoesNotExistError ioe then Just ioe else Nothing)
      (\_ -> return e)

-- | Run an IO computation, returning @e@ if there is an 'error'
-- call. ('ErrorCall')
handleErrorCall :: a -> IO a -> IO a
handleErrorCall e =
    handle (\(ErrorCall _) -> return e)

------------------------------------------------------------------------------
-- Instances
-- 

instance Text FilePathGlob where
  disp (GlobDir  glob pathglob) = disp glob Disp.<> Disp.char '/'
                                            Disp.<> disp pathglob
  disp (GlobFile glob)          = disp glob

  parse = parse >>= \glob ->
            (do _ <- ReadP.char '/'
                globs <- parse
                return (GlobDir glob globs))
        <++ return (GlobFile glob)

instance Binary UTCTime where
  put (UTCTime (ModifiedJulianDay day) tod) = do
    put day
    put (toRational tod)
  get = do
    day  <- get
    tod <- get
    return $! UTCTime (ModifiedJulianDay day)
                      (fromRational tod)

instance Binary MonitorStateFileSet where
  put (MonitorStateFileSet singlePaths globPaths) = do
    put (1 :: Int) -- version
    put singlePaths
    put globPaths
  get = do
    ver <- get
    if ver == (1 :: Int)
      then do singlePaths <- get
              globPaths   <- get
              return $! MonitorStateFileSet singlePaths globPaths
      else fail "MonitorStateFileSet: wrong version"


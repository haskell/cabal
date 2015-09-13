-- | A cache which tracks a value whose validity depends upon
-- the state of various files in the filesystem.

module Distribution.Client.FileStatusCache where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as BS
import           Data.Binary
import qualified Data.Binary as Binary
import           Data.Hashable
import           Data.Time (UTCTime(..), Day(..))
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Reader
import           Control.Exception

import           System.FilePath
import           System.Directory
import           System.IO
import           System.IO.Error


-- contains a mapping from normalised relative paths to timestamp and content hash.
newtype FileStatusCache = FileStatusCache (Map FilePath FileInfo)

data FileInfo = FileInfo {-# UNPACK #-} !UTCTime
                         {-# UNPACK #-} !Hash
              | NoFileExists -- meaning, file expected not to exist

type Hash = Int

emptyFileStatusCache :: FileStatusCache
emptyFileStatusCache = FileStatusCache Map.empty

-- | A piece of a globbing pattern
data GlobAtom = WildCard
              | Literal String
              | Union [Glob]

-- | A single directory or file component of a globbed path
newtype Glob = Glob [GlobAtom]

-- | A path specified by file globbing
data GlobPath = Directory [Glob] GlobPath
              | File [Glob]

-- | A list of search paths
type SearchPaths = [FilePath]

-- | A description of the the files our cache is interested in.
data FileSpec = SingleHashedFile FilePath
              | SingleFile FilePath
              | GlobHashPath GlobPath
                -- ^ Look for a file matching a glob pattern, checking
                -- its hash for changes if necessary.
              | SearchPath SearchPaths FilePath
                -- ^ Find the first occurrence of the file with the given
                -- file name in the given list of directories

data CachedGlobPath = CDirectory [Glob] CachedGlobPath ModTime [FilePath]
                    | CFile [Glob] ModTime Hash

data CachedFileSpec = CSingleHashedFile ModTime Hash
                    | CSingleFile ModTime
                    | CGlobHashPath CachedGlobPath
                    | CSearchPath [FilePath] FilePath (Maybe (FilePath, ModTime))

data FileStatusCache a = FileStatusCache a (Map FilePath CachedFileSpec)

type ChangedT m b = MaybeT (ReaderT b m)

-- | @a `dropPrefix` b@ tests whether @b@ is a prefix of @a@,
-- return @Just@ the remaining portion of @a@ if so.
dropPrefix :: String -> String -> Maybe String
dropPrefix xs     []     = Just xs
dropPrefix (x:xs) (y:ys)
  | x == y               = dropPrefix xs ys
dropPrefix _      _      = Nothing

-- | Test whether a name matches a globbing pattern
globMatches :: Glob -> String -> Bool
globMatches [] ""                 = True
globMatches (Literal lit:rest) s
  | Just s' <- s `dropPrefix` lit = globMatches rest s'
globMatches [WildCard] ""         = True
globmatches (WildCard:rest) xs    =
    globMatches rest xs || globmatches (WildCard:rest) (tail xs)

runChangedT :: b -> ChangedT m b () -> m (Changed b)
runChangedT cached action =
    maybe Changed NotChanged res <$> runReaderT (runMaybeT action) cached

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
      Nothing -> return True
      Just (FileStatusCache cachedValued fileCache) ->
        runMaybeOrChanged
        $ runStateT (probe cachedValue (M.fromList fileCache)) fileCache
  where
    probe :: [(FilePath, CachedFileSpec)]
          -> StateT FileStatusCache (MaybeT (ReaderT a IO)) (Changed a)
    probe [] = Unchanged `fmap` ask
    probe ((file, CSingleFile mtime) : specs) = do
      same <- liftIO $ probeModificationTime file mtime
      if same
        then probe specs
        else return Changed

    probe ((file, CSingleHashedFile mtime hash) : specs) = do
      sameMTime <- liftIO $ probeModificationTime file mtime
      if sameMTime
        then probe specs
        else do sameHash <- liftIO $ probeFileHash file hash
                if sameHash
                  then probe specs
                  else return Changed

    probe ((file, CGlobHashPath globpath) : specs) = do
      same <- liftIO $ probeGlobPath globpath
      if same
        then probe specs
        else return Changed

    probe ((file, CSearchPath search name file) : specs) = do
      file' <- liftIO $ findFile search name
      if file == file'
        then case file of
               Just (path, mtime) -> do
                 same <- liftIO $ probeModificationTime path mtime
                 if same
                   then probe specs
                   else return Changed
               Nothing -> return Changed
        else return Changed

    probeGlobPath :: CachedGlobPath
                  -> StateT FileStatusCache (MaybeT (ReaderT a IO)) (Changed a)
    probeGlobPath (CDirectory globs rest mtime children) = do
    probeGlobPath (CFile globs mtime hash) = do
      f

    probeModificationTime :: FilePath -> ModTime -> IO Bool
    probeModificationTime file mtime =
      handleDoesNotExist (\_ -> return True) $ do
       mtime' <- getModificationTime file
       return (mtime == mtime')

    probeFileHash file chash =
      handleDoesNotExist (\_ -> return True) $ do
        chash' <- readFileHash file

        --TODO: debug only:
        when (chash == chash') $
          print ("checkFileStatusChanged", file, chash, chash')

        return (chash == chash')

updateFileStatusCache :: FilePath   -- ^ cache file path
                      -> FilePath   -- ^ root directory
                      -> [FileSpec] -- ^ patterns of interest relative to
                                    -- root
                      -> a          -- ^ a cached value dependent upon the
                                    -- paths identified by the given patterns
                      -> IO ()      -- ^ did any of these paths change

-- | Given a 'FileStatusCache' and a set of files that we are interested in,
-- check if any of those files have changed. The set of files and the cache
-- are both taken to be relative to the given root directory.
--
-- A change here includes additions or deletions compared to the set of files
-- that we are interested in, and any change in content.
--
-- To make this a bit faster it relies on an assumption: files where the
-- modification timestamps have not chagnged are assumed to have not changed
-- content. On the other hand, if there is change in modification timestamp, a
-- file content hash is checked to see if it's really changed. So in the
-- typical case only file timestamps need to be checked.
--
checkFileStatusChanged :: FilePath -> FilePath -> IO Bool
checkFileStatusChanged dir statusCacheFile = do
    mfileCache <- handleDoesNotExist (\_ -> return Nothing) $
                    either (\_ -> return Nothing)
                           (return . Just)
                       =<< Binary.decodeFileOrFail statusCacheFile
    case mfileCache of
      Nothing -> return True
      Just (FileStatusCache fileCache) ->
        -- Assume the files we are interested in are all those in the cache.
        -- So, we go and probe them all.
        probe (Map.toList fileCache)
  where
    probe []                                      = return False
    probe ((relfile, NoFileExists         :files) = do
      let file = dir </> relfile
      exists <- doesFileExist file -- or dir?
      if not exists
        then probe files
        else return True
      
    probe ((relfile, FileInfo mtime chash):files) = do
      let file = dir </> relfile
      samemtime <- probeModificationTime file mtime
      if samemtime
        then probe files
        -- Only read the file and calculate the hash if the mtime changed
        else do
          samechash <- probeFileHash file chash
          if samechash
            then probe files
            else return True

    probeModificationTime file mtime =
      handleDoesNotExist (\_ -> return True) $ do
       mtime' <- getModificationTime file
       return (mtime == mtime')

    probeFileHash file chash =
      handleDoesNotExist (\_ -> return True) $ do
        chash' <- readFileHash file

        --TODO: debug only:
        when (chash == chash') $
          print ("checkFileStatusChanged", file, chash, chash')

        return (chash == chash')
{-
checkFileStatusChanged :: FilePath -> FileStatusCache -> Set FilePath -> IO Bool
checkFileStatusChanged dir (FileStatusCache fileCache) knownFiles

    -- First check if the cache mentions all the files and only the files we
    -- are interested in. If so, we go on to probe them all.
    | Map.keysSet fileCache == knownFiles
    = probe (Map.toList (fileCache `Map.intersection` knownFiles'))

    -- If not, we can bail out immediately as something has changed.
    | otherwise
    = return True
  where
    knownFiles' = Map.fromSet (const ()) knownFiles

    probe []                                      = return False
    probe ((relfile, FileInfo mtime chash):files) = do
      let file = dir </> relfile
      mtime' <- getModificationTime file
      if mtime == mtime'
        then probe files
        else do
          -- Only read the file and calculate the hash if the mtime changed
          chash' <- readFileHash file
          if chash == chash'
            then probe files
            else return True
-}


updateFileStatusCache :: FilePath -> FilePath -> [FilePath] -> IO ()
updateFileStatusCache dir statusCacheFile knownFiles = do
    FileStatusCache fileCacheOld <-
      handleDoesNotExist (\_ -> return emptyFileStatusCache) $
        either (\_ -> return emptyFileStatusCache)
               return
           =<< Binary.decodeFileOrFail statusCacheFile

    -- Go over each of the knownFiles, joined with the old cache
    -- and return up to date file info, reusing the cache where possible
    fileCache <- Map.traverseWithKey probe $
                   leftJoinWith (\_ -> Nothing) (\_ finfo -> Just finfo)
                                (Map.fromList (map (\k->(k,())) knownFiles))
                                fileCacheOld

    Binary.encodeFile statusCacheFile (FileStatusCache fileCache)
  where
    probe :: FilePath -> Maybe FileInfo -> IO FileInfo
    probe relfile Nothing = do
      -- new file, have to get file info
      let file = dir </> relfile
      readFileInfo file

    probe relfile (Just finfo@(FileInfo mtime _chash)) = do
      -- existing file, check file info and update if necessary
      let file = dir </> relfile
      mtime' <- getModificationTime file
      if mtime == mtime'
        then return finfo
        else readFileInfo file

leftJoinWith :: Ord k => (a -> c) -> (a -> b -> c)
             -> Map k a -> Map k b -> Map k c
leftJoinWith left middle =
    Map.mergeWithKey (\_ a b -> Just $! middle a b)  -- join the inner
                     (Map.map left)                  -- include the left outer
                     (const Map.empty)               -- drop the right

readFileHash :: FilePath -> IO Hash
readFileHash file =
    withBinaryFile file ReadMode $ \hnd ->
      evaluate . hash =<< BS.hGetContents hnd
    
readFileInfo :: FilePath -> IO FileInfo
readFileInfo file =
    FileInfo <$> getModificationTime file
             <*> readFileHash file

handleDoesNotExist :: (IOError -> IO a) -> IO a -> IO a
handleDoesNotExist =
    handleJust (\ioe -> if isDoesNotExistError ioe then Just ioe else Nothing)

instance Binary FileInfo where
  put (FileInfo a b) = do put a >> put b
  get = do a <- get; b <- get; return $! FileInfo a b

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
  put (FileStatusCache fileCache) = do
    put (0 :: Int) -- version
    put fileCache
  get = do
    ver <- get
    if ver == (0 :: Int)
      then do fileCache <- get
              return $! FileStatusCache fileCache
      -- note, it's ok if the format is wrong to just return empty
      else return emptyFileStatusCache

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


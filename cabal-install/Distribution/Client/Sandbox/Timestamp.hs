-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Timestamp
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Timestamp file handling (for add-source dependencies).
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Timestamp (
  AddSourceTimestamp,
  withAddTimestamps,
  withRemoveTimestamps,
  withUpdateTimestamps,
  maybeAddCompilerTimestampRecord,
  isDepModified,
  withModifiedDeps,
  ) where

import Control.Exception                             (finally)
import Control.Monad                                 (filterM, forM, when)
import Data.Char                                     (isSpace)
import Data.List                                     (partition)
import System.Directory                              (removeFile, renameFile)
import System.FilePath                               ((<.>), (</>))

import Distribution.Compiler                         (CompilerId)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse         (readPackageDescription)
import Distribution.Simple.Setup                     (Flag (..),
                                                      SDistFlags (..),
                                                      defaultSDistFlags,
                                                      sdistCommand)
import Distribution.Simple.Utils                     (debug, die,
                                                      findPackageDesc, warn)
import Distribution.System                           (Platform)
import Distribution.Text                             (display)
import Distribution.Verbosity                        (Verbosity, lessVerbose,
                                                      normal)
import Distribution.Version                          (Version (..),
                                                      orLaterVersion)

import Distribution.Client.Sandbox.Index
  (ListIgnoredBuildTreeRefs (DontListIgnored), RefTypesToList(OnlyLinks)
  ,listBuildTreeRefs)
import Distribution.Client.SetupWrapper              (SetupScriptOptions (..),
                                                      defaultSetupScriptOptions,
                                                      setupWrapper)
import Distribution.Client.Utils                     (inDir,
                                                      tryCanonicalizePath)

import Distribution.Compat.Exception                 (catchIO)
import Distribution.Compat.Time                      (EpochTime, getCurTime,
                                                      getModTime)


-- | Timestamp of an add-source dependency.
type AddSourceTimestamp  = (FilePath, EpochTime)
-- | Timestamp file record - a string identifying the compiler & platform plus a
-- list of add-source timestamps.
type TimestampFileRecord = (String, [AddSourceTimestamp])

timestampRecordKey :: CompilerId -> Platform -> String
timestampRecordKey compId platform = display platform ++ "-" ++ display compId

-- | The 'add-source-timestamps' file keeps the timestamps of all add-source
-- dependencies. It is initially populated by 'sandbox add-source' and kept
-- current by 'reinstallAddSourceDeps' and 'configure -w'. The user can install
-- add-source deps manually with 'cabal install' after having edited them, so we
-- can err on the side of caution sometimes.
-- FIXME: We should keep this info in the index file, together with build tree
-- refs.
timestampFileName :: FilePath
timestampFileName = "add-source-timestamps"

-- | Read the timestamp file. Returns an empty list if the file doesn't exist.
readTimestampFile :: FilePath -> IO (Maybe [TimestampFileRecord])
readTimestampFile timestampFile = do
  timestampString <- readFile timestampFile `catchIO` \_ -> return "[]"
  case reads timestampString of
    [(timestamps, s)] | all isSpace s -> return (Just timestamps)
    _                                 -> return Nothing

-- | Write the timestamp file, atomically.
writeTimestampFile :: FilePath -> [TimestampFileRecord] -> IO ()
writeTimestampFile timestampFile timestamps = do
  writeFile  timestampTmpFile (show timestamps)
  renameFile timestampTmpFile timestampFile
  where
    timestampTmpFile = timestampFile <.> "tmp"

-- | Read, process and write the timestamp file in one go.
withTimestampFile :: FilePath
                     -> ([TimestampFileRecord] -> IO [TimestampFileRecord])
                     -> IO ()
withTimestampFile sandboxDir process = do
  let timestampFile  = sandboxDir </> timestampFileName
  mTimestampRecords <- readTimestampFile timestampFile
  case mTimestampRecords of
    Nothing               -> die $ "The timestamps file is corrupted. "
                                   ++ "Please delete & recreate the sandbox."
    Just timestampRecords -> do
      timestampRecords' <- process timestampRecords
      writeTimestampFile timestampFile timestampRecords'

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've added and the current time, add an 'AddSourceTimestamp' to the list for
-- each path that isn't already included.
addTimestamps :: EpochTime -> [AddSourceTimestamp] -> [FilePath]
                 -> [AddSourceTimestamp]
addTimestamps now timestamps paths =
  map (\p -> (p, now)) newPaths ++ timestamps
  where
    oldPaths      = map fst timestamps
    (_, newPaths) = partition (flip elem oldPaths) paths

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've reinstalled and a new timestamp value, update the timestamp value for
-- the deps in the list. If there are new paths in the list, ignore them.
updateTimestamps :: [AddSourceTimestamp] -> [FilePath] -> EpochTime
                    -> [AddSourceTimestamp]
updateTimestamps timestamps pathsToUpdate newTimestamp =
  foldr updateTimestamp [] timestamps
  where
    updateTimestamp t@(path, _oldTimestamp) rest
      | path `elem` pathsToUpdate = (path, newTimestamp) : rest
      | otherwise                 = t : rest

-- | Given a list of 'TimestampFileRecord's and a list of paths to add-source
-- deps we've removed, remove those deps from the list.
removeTimestamps :: [AddSourceTimestamp] -> [FilePath] -> [AddSourceTimestamp]
removeTimestamps l pathsToRemove = foldr removeTimestamp [] l
  where
    removeTimestamp t@(path, _oldTimestamp) rest =
      if path `elem` pathsToRemove
      then rest
      else t : rest

-- | If a timestamp record for this compiler doesn't exist, add a new one.
maybeAddCompilerTimestampRecord :: Verbosity -> FilePath -> FilePath
                                   -> CompilerId -> Platform
                                   -> IO ()
maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
                                compId platform = do
  buildTreeRefs <- listBuildTreeRefs verbosity DontListIgnored OnlyLinks
                                     indexFile
  withTimestampFile sandboxDir $ \timestampRecords -> do
    let key = timestampRecordKey compId platform
    case lookup key timestampRecords of
      Just _  -> return timestampRecords
      Nothing -> do now <- getCurTime
                    let timestamps = map (\p -> (p, now)) buildTreeRefs
                    return $ (key, timestamps):timestampRecords

-- | Given an IO action that returns a list of build tree refs, add those
-- build tree refs to the timestamps file (for all compilers).
withAddTimestamps :: FilePath -> IO [FilePath] -> IO ()
withAddTimestamps sandboxDir act = do
  now <- getCurTime
  withActionOnAllTimestamps (addTimestamps now) sandboxDir act

-- | Given an IO action that returns a list of build tree refs, remove those
-- build tree refs from the timestamps file (for all compilers).
withRemoveTimestamps :: FilePath -> IO [FilePath] -> IO ()
withRemoveTimestamps = withActionOnAllTimestamps removeTimestamps

-- | Given an IO action that returns a list of build tree refs, update the
-- timestamps of the returned build tree refs to the current time (only for the
-- given compiler & platform).
withUpdateTimestamps :: FilePath -> CompilerId -> Platform
                        ->([AddSourceTimestamp] -> IO [FilePath])
                        -> IO ()
withUpdateTimestamps =
  withActionOnCompilerTimestamps updateTimestamps

-- | Helper for implementing 'withAddTimestamps' and
-- 'withRemoveTimestamps'. Runs a given action on the list of
-- 'AddSourceTimestamp's for all compilers, applies 'f' to the result and then
-- updates the timestamp file. The IO action is run only once.
withActionOnAllTimestamps :: ([AddSourceTimestamp] -> [FilePath]
                              -> [AddSourceTimestamp])
                             -> FilePath
                             -> IO [FilePath]
                             -> IO ()
withActionOnAllTimestamps f sandboxDir act =
  withTimestampFile sandboxDir $ \timestampRecords -> do
    paths <- act
    return [(key, f timestamps paths) | (key, timestamps) <- timestampRecords]

-- | Helper for implementing 'withUpdateTimestamps'. Runs a given action on the
-- list of 'AddSourceTimestamp's for this compiler, applies 'f' to the result
-- and then updates the timestamp file record. The IO action is run only once.
withActionOnCompilerTimestamps :: ([AddSourceTimestamp]
                                   -> [FilePath] -> EpochTime
                                   -> [AddSourceTimestamp])
                                  -> FilePath
                                  -> CompilerId
                                  -> Platform
                                  -> ([AddSourceTimestamp] -> IO [FilePath])
                                  -> IO ()
withActionOnCompilerTimestamps f sandboxDir compId platform act = do
  let needle = timestampRecordKey compId platform
  withTimestampFile sandboxDir $ \timestampRecords -> do
    timestampRecords' <- forM timestampRecords $ \r@(key, timestamps) ->
      if key == needle
      then do paths <- act timestamps
              now   <- getCurTime
              return (key, f timestamps paths now)
      else return r
    return timestampRecords'

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
-- FIXME: This function is not thread-safe because of 'inDir'.
allPackageSourceFiles :: Verbosity -> FilePath -> IO [FilePath]
allPackageSourceFiles verbosity packageDir = inDir (Just packageDir) $ do
  pkg <- fmap (flattenPackageDescription)
         . readPackageDescription verbosity =<< findPackageDesc packageDir

  let file      = "cabal-sdist-list-sources"
      flags     = defaultSDistFlags {
        sDistVerbosity   = Flag $ if verbosity == normal
                                  then lessVerbose verbosity else verbosity,
        sDistListSources = Flag file
        }
      setupOpts = defaultSetupScriptOptions {
        -- 'sdist --list-sources' was introduced in Cabal 1.17.
        useCabalVersion = orLaterVersion $ Version [1,17,0] []
        }

  -- Run setup sdist --list-sources=TMPFILE
  (flip finally) (removeFile file) $ do
    setupWrapper verbosity setupOpts (Just pkg) sdistCommand (const flags) []
    srcs <- fmap lines . readFile $ file
    mapM tryCanonicalizePath srcs

-- | Has this dependency been modified since we have last looked at it?
isDepModified :: Verbosity -> EpochTime -> AddSourceTimestamp -> IO Bool
isDepModified verbosity now (packageDir, timestamp) = do
  depSources <- allPackageSourceFiles verbosity packageDir
  go depSources

  where
    go []         = return False
    go (dep:rest) = do
      -- FIXME: What if the clock jumps backwards at any point? For now we only
      -- print a warning.
      modTime <- getModTime dep
      when (modTime > now) $
        warn verbosity $ "File '" ++ dep
                         ++ "' has a modification time that is in the future."
      if modTime >= timestamp
        then do
          debug verbosity ("Dependency has a modified source file: " ++ dep)
          return True
        else go rest

-- | Given an IO action, feed to it the list of modified add-source deps and
-- set their timestamps to the current time in the timestamps file.
withModifiedDeps :: Verbosity -> FilePath -> CompilerId -> Platform
                    -> ([FilePath] -> IO ()) -> IO ()
withModifiedDeps verbosity sandboxDir compId platform act = do
  withUpdateTimestamps sandboxDir compId platform $ \timestamps -> do
    now <- getCurTime
    modified <- fmap (map fst) . filterM (isDepModified verbosity now)
                $ timestamps
    act modified
    return modified

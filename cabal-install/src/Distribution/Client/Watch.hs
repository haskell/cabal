{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Support for the @--watch@ flag: compute the set of files relevant to a
-- command's targets, watch them and re-run the command on change.
--
-- Limitations: the watch set is computed once per invocation.  A file created
-- in an already-watched source directory triggers a re-run (see
-- 'watchCreateExtensions'), but brand-new directories require restarting the
-- command.  Editor save-via-rename events that never surface an @Added@ event
-- for a watched name may be missed until the next event.  Events that arrive
-- while the command runs are coalesced but not bounded; a source directory
-- with a very high event rate can grow memory until the command finishes.
-- A watched directory that is deleted and recreated silently stops being
-- watched.  A re-run repeats the command's post-planning phases with the
-- plan elaborated at startup: edits to @.cabal@ or @cabal.project@ files
-- trigger a re-run, but a re-run that only changes the build configuration
-- (e.g. new modules in @exposed-modules@) is not picked up until the
-- command is restarted.  For @cabal install --watch@ the watched project
-- files live in the planning directory, not the user's project root, so
-- only the per-package sources and @.cabal@ files are effectively watched.
module Distribution.Client.Watch
  ( watchFiles
  , projectWatchFiles
  , planWatchFiles
  , watchLoop
  , watchEventRelevant
  , watchCreateExtensions
  , cabalFilesInDir
  , WatchEventInfo
  ) where

import Distribution.Client.Compat.Prelude hiding (first)
import Distribution.Utils.Generic (ordNub)
import Prelude ()

import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , ProjectFileKey (..)
  )
import Distribution.Client.Errors (CabalInstallException (..))
import Distribution.Client.FileMonitor (MonitorFilePath (..))
import Distribution.Client.Glob
  ( isTrivialRootedGlob
  )
import Distribution.Client.ProjectPlanning.Types
  ( ElaboratedConfiguredPackage
  , elabPkgSourceLocation
  )
import Distribution.Client.RebuildMonad (execRebuild)
import Distribution.Client.SourceFiles (needElaboratedConfiguredPackage)
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Simple.Utils (dieWithException, notice)

import Control.Exception (AsyncException (..), try)
import System.Directory (listDirectory)
import System.FilePath
  ( takeExtension
  , takeFileName
  , (</>)
  )

#if defined(linux_HOST_OS) && defined(HAVE_FSWATCH)
import Control.Concurrent.STM
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Directory (makeAbsolute)
import System.FSNotify
  ( Event (..)
  , eventPath
  , watchDir
  , withManager
  )
import System.FilePath (normalise, takeDirectory)
import System.Timeout (timeout)
#endif

-- | @(@path@, @is a create event@)@ abstraction of an fsnotify event,
-- kept platform-independent so the relevance predicate is testable
-- everywhere.
type WatchEventInfo = (FilePath, Bool)

-- | File extensions for which a @create@ event in an already-watched source
-- directory triggers a re-run (handles newly created module files).
watchCreateExtensions :: [String]
watchCreateExtensions =
  [ ".hs"
  , ".lhs"
  , ".hsig"
  , ".hsc"
  , ".c"
  , ".cpp"
  , ".h"
  , ".cc"
  , ".cxx"
  , ".cabal"
  ]

-- | The three project files: the main @cabal.project@ and its @.local@ and
-- @.freeze@ variants.  Watched unconditionally, whether or not they exist.
projectWatchFiles :: DistDirLayout -> [FilePath]
projectWatchFiles layout =
  map
    (distProjectFile layout)
    [ProjectFileKeyMain, ProjectFileKeyLocal, ProjectFileKeyFreeze]

-- | The @.cabal@ files in the given package directory.  'SourceFiles' does
-- not monitor the package's own @.cabal@ file, so watch it separately.
cabalFilesInDir :: FilePath -> IO [FilePath]
cabalFilesInDir dir = do
  dirContents <- listDirectory dir
  pure [dir </> f | f <- dirContents, ".cabal" == takeExtension f]

-- | The files relevant to the given local (project-internal) elaborated
-- packages: the project files plus the per-package source files and
-- @.cabal@ files.  Dependencies live in the store and are not watched.
planWatchFiles
  :: Verbosity
  -> DistDirLayout
  -> [ElaboratedConfiguredPackage]
  -> IO [FilePath]
planWatchFiles _verbosity layout elabs = do
  perPkg <- traverse pkgFiles elabs
  pure . sort . ordNub $ (projectWatchFiles layout ++ concat perPkg)
  where
    pkgFiles :: ElaboratedConfiguredPackage -> IO [FilePath]
    pkgFiles elab = case elabPkgSourceLocation elab of
      LocalUnpackedPackage dir -> do
        monitors <- execRebuild dir (needElaboratedConfiguredPackage elab)
        cabalFiles <- cabalFilesInDir dir
        let srcFiles = concatMap (monitorToPaths dir) monitors
        pure (srcFiles ++ cabalFiles)
      LocalTarballPackage f -> pure [f]
      _ -> pure []

-- | Convert the file monitors recorded by 'needElaboratedConfiguredPackage'
-- (relative to @srcdir@) into concrete paths to watch.  'MonitorFileGlob'
-- never occurs today; defensively we watch the glob's rooted directory.
monitorToPaths :: FilePath -> MonitorFilePath -> [FilePath]
monitorToPaths srcdir = \case
  MonitorFile _ _ p -> [srcdir </> p]
  MonitorFileGlob _ _ rglob -> case isTrivialRootedGlob rglob of
    Just p -> [srcdir </> p]
    -- Fall back to the source directory itself.  'filesToDirWatches' splits
    -- paths into (parent, basename) pairs, so a bare directory path would
    -- watch the wrong (parent) directory; the wildcard name never matches a
    -- real file, leaving the create-extension rule to fire for new modules.
    Nothing -> [srcdir </> "*"]

-- | Is a file-system event relevant given the file names watched in the
-- event's directory?  An event is relevant if it touches a watched name, or
-- if it creates a file with one of the 'watchCreateExtensions' extensions
-- (so that newly written modules trigger a rebuild).
watchEventRelevant :: [FilePath] -> WatchEventInfo -> Bool
watchEventRelevant names (path, isCreate) =
  fileName `elem` names
    || (isCreate && takeExtension path `elem` watchCreateExtensions)
  where
    fileName = takeFileName path

#if defined(linux_HOST_OS) && defined(HAVE_FSWATCH)
-- | A watch specification: a directory plus the set of watched file names
-- within it.
type DirWatch = (FilePath, [FilePath])

groupDirWatches :: [DirWatch] -> [DirWatch]
groupDirWatches =
  Map.toAscList . foldl' merge Map.empty
  where
    merge m (dir, names) =
      Map.insertWith (\new old -> ordNub (new ++ old)) dir names m

filesToDirWatches :: [FilePath] -> [DirWatch]
filesToDirWatches files =
  [ (takeDirectory f, [takeFileName f])
  | f <- files
  ]
#endif

-- | Run @cmd@ once, then re-run it whenever the event source yields a change,
-- until the event source is exhausted (@Nothing@).  A failing command
-- (including one exiting nonzero) is reported but does not stop the loop;
-- a Ctrl-C (@UserInterrupt@) while the command runs propagates.
watchLoop :: Verbosity -> IO (Maybe FilePath) -> IO () -> IO ()
watchLoop verbosity nextEvent cmd = do
  cmd
  loop
  where
    loop = do
      mnext <- nextEvent
      case mnext of
        Nothing -> pure ()
        Just path -> do
          notice verbosity ("File " ++ path ++ " changed, re-running...")
          -- Catch synchronous exceptions (including 'ExitCode' from
          -- 'exitFailure' and friends); rethrow a user interrupt.
          r <- try cmd
          case (r :: Either SomeException ()) of
            Right () -> pure ()
            Left se -> case fromException se of
              Just UserInterrupt -> throwIO se
              _ -> keepWatching
          loop
    keepWatching =
      notice verbosity "Command failed; keeping watch, re-running on next change."

-- | Watch the given files (plus their directories for new files) and run
-- @cmd@ whenever one of them changes, until interrupted.  Only supported on
-- Linux (or any platform with fsnotify support enabled at build time).
watchFiles :: Verbosity -> [FilePath] -> IO () -> IO ()
#if defined(linux_HOST_OS) && defined(HAVE_FSWATCH)
watchFiles verbosity files cmd = do
  absFiles <- traverse (fmap normalise . makeAbsolute) files
  -- If the watcher cannot be started (e.g. the inotify watch limit is
  -- exhausted), fail with a proper cabal error instead of an fsnotify
  -- exception.  A user interrupt propagates.  Once the watches are
  -- established, any exception comes from the watched command and is
  -- rethrown verbatim rather than mislabeled as a watcher-start failure.
  startedRef <- newIORef False
  r <- try $ withManager $ \mgr -> do
    chan <- newTChanIO
    let dirWatches = groupDirWatches (filesToDirWatches absFiles)
    for_ dirWatches $ \(dir, names) ->
      void $
        watchDir
          mgr
          dir
          ( \ev ->
              watchEventRelevant
                names
                ( eventPath ev
                , case ev of
                    Added{} -> True
                    _ -> False
                )
          )
          (\ev -> atomically (writeTChan chan (eventPath ev)))
    writeIORef startedRef True
    -- Debounce: after the first event, coalesce any further events arriving
    -- within 'watchDebounceDelay' microseconds into one re-run.
    let nextEvent = do
          first <- atomically (readTChan chan)
          later <- drain first
          pure (Just later)
        drain acc = do
          m <- timeout watchDebounceDelay (atomically (readTChan chan))
          case m of
            Just p -> drain p
            Nothing -> pure acc
    watchLoop verbosity nextEvent cmd
  case (r :: Either SomeException ()) of
    Right () -> pure ()
    Left se -> case fromException se of
      Just UserInterrupt -> throwIO se
      _ -> do
        started <- readIORef startedRef
        if started
          then throwIO se
          else dieWithException verbosity (WatchStartFailed (show se))

watchDebounceDelay :: Int
watchDebounceDelay = 100000
#else
watchFiles verbosity _files _cmd =
  dieWithException verbosity (WatchUnsupportedOnPlatform "watching requires Linux with fsnotify support")
#endif

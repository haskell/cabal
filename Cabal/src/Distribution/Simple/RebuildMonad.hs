{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | An abstraction for re-running actions if values or files have changed.
--
-- This is not a full-blown make-style incremental build system, it's a bit
-- more ad-hoc than that, but it's easier to integrate with existing code.
--
-- It's a convenient interface to the "Distribution.Client.FileMonitor"
-- functions.
module Distribution.Simple.RebuildMonad
  ( -- * MonadRebuild class
    MonadRebuild (..)

    -- * Setting up file monitoring
  , monitorFiles
  , MonitorFilePath
  , monitorFile
  , monitorFileHashed
  , monitorNonExistentFile
  , monitorDirectory
  , monitorNonExistentDirectory
  , monitorDirectoryExistence
  , monitorFileOrDirectory
  , monitorFileSearchPath
  , monitorFileHashedSearchPath

    -- ** Monitoring file globs
  , monitorFileGlob
  , monitorFileGlobExistence
  , RootedGlob (..)
  , FilePathRoot (..)

    -- * Using a file monitor
  , FileMonitor (..)
  , newFileMonitor
  , rerunIfChanged'

    -- * Utils
  , delayInitSharedResource
  , delayInitSharedResources
  , getDirectoryContentsMonitored
  , createDirectoryMonitored
  , monitorDirectoryStatus
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.FileMonitor
import Distribution.Verbosity (Verbosity (..))

import Distribution.Simple.Utils (debug)

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Monad
import Control.Monad.Writer as Writer
import qualified Data.Map.Strict as Map
import System.Directory
import System.FilePath

-- | A monad layered on top of 'IO' to help with re-running actions when the
-- input files and values they depend on change. The crucial operations are
-- 'rerunIfChanged'' and 'monitorFiles'.
class
  (Monad m, MonadIO m, MonadWriter [MonitorFilePath] m) =>
  MonadRebuild m
  where
  -- | Lift an 'IO' action into a 'MonadRebuild' context, such that other
  -- (compatible) 'MonadRebuild' actions can be unlifted inside that 'IO'
  -- action. This is similar to @MonadUnliftIO@ or @MonadBaseControl@, but
  -- specifically designed to support the needs of 'MonadRebuild': collecting
  -- dependent file paths to monitor, providing 'liftIO', and allowing
  -- instances to inject additional monadic context (@MonadReader@ or
  -- equivalent).
  --
  -- The usage pattern for this looks something like this:
  --
  -- > foobar :: MonadRebuild m => m ResultType
  -- > foobar = do
  -- >   -- We are in the 'm' monad here
  -- >   withRunRebuildInIO $ \runInIO -> do
  -- >     -- Now we are in 'IO'
  -- >     x <- doSomethingInIO
  -- >     (result, files) <- runInIO $ do
  -- >       -- We are back in 'm'; the result of 'someRebuildAction' becomes
  -- >       -- the first tuple element of the 'runInIO' return type, while any
  -- >       -- file monitoring paths become the second element.
  -- >       someRebuildAction x
  -- >     result' <- someIOProcessing result
  -- >     -- Return the result and the files that resulted from the inner 'm'
  -- >     -- action; the result will become the result of the outer 'm' action,
  -- >     -- while the files get injected back into the 'm' monad.
  -- >     return (result', files)
  withRunRebuildInIO
    :: ((forall a. m a -> IO (a, [MonitorFilePath])) -> IO (b, [MonitorFilePath]))
    -> m b

-- | Minimal instance for a simple @WriterT [MonitorFilePath] IO@.
instance MonadRebuild (WriterT [MonitorFilePath] IO) where
  withRunRebuildInIO inner = do
    (result, files) <- liftIO $ inner runWriterT
    Writer.tell files
    return result

-- | Use this within the body action of 'rerunIfChanged' to declare that the
-- action depends on the given files. This can be based on what the action
-- actually did. It is these files that will be checked for changes next
-- time 'rerunIfChanged' is called for that 'FileMonitor'.
--
-- Relative paths are interpreted as relative to an implicit root, ultimately
-- passed in to 'runRebuild'.
monitorFiles :: MonadRebuild m => [MonitorFilePath] -> m ()
monitorFiles filespecs =
  Writer.tell filespecs

-- | This captures the standard use pattern for a 'FileMonitor': given a
-- monitor, an action and the input value the action depends on, either
-- re-run the action to get its output, or if the value and files the action
-- depends on have not changed then return a previously cached action result.
--
-- The result is still in the original 'MonadRebuild' monad, so these can be
-- nested.
--
-- Do not share 'FileMonitor's between different uses of 'rerunIfChanged''.
rerunIfChanged'
  :: (Binary a, Structured a, Binary b, Structured b, MonadRebuild m)
  => Verbosity
  -> FilePath
  -- ^ Root directory for file monitoring. Used to resolve relative paths
  -- in file monitors.
  -> ([IO (b, [MonitorFilePath])] -> IO [(b, [MonitorFilePath])])
  -- ^ Helper function for combining multiple IO actions into a single one.
  -- For serial execution, this can simply be 'sequence'; for concurrent
  -- execution, something more sophisticated can be passed here.
  -> [(FileMonitor a b, a, m b)]
  -- ^ Triples of a file monitor, a key to identify it, and an associated
  -- action.
  -> m [b]
rerunIfChanged' verbosity rootDir chainIOs triples = do
  withRunRebuildInIO $ \runInIO -> do
    dacts <- forM triples $ \(monitor, key, action) -> do
      let monitorName = takeFileName (fileMonitorCacheFile monitor)
      changed <- liftIO $ checkFileMonitorChanged monitor rootDir key
      case changed of
        MonitorUnchanged result files -> do
          debug verbosity $
            "File monitor '"
              ++ monitorName
              ++ "' unchanged."
          ((), files') <- runInIO $ monitorFiles files
          return (return (result, files'))
        MonitorChanged reason -> do
          debug verbosity $
            "File monitor '"
              ++ monitorName
              ++ "' changed: "
              ++ showReason reason
          return $ do
            startTime <- beginUpdateFileMonitor
            (result, files) <- runInIO action
            updateFileMonitor
              monitor
              rootDir
              (Just startTime)
              files
              key
              result
            return (result, files)

    (results, files) <- unzip <$> chainIOs dacts
    return (results, mconcat files)
  where
    showReason (MonitoredFileChanged file) = "file " ++ file
    showReason (MonitoredValueChanged _) = "monitor value changed"
    showReason MonitorFirstRun = "first run"
    showReason MonitorCorruptCache = "invalid cache file"

-- | When using 'rerunIfChanged' for each element of a list of actions, it is
-- sometimes the case that each action needs to make use of some resource. e.g.
--
-- > sequence
-- >   [ rerunIfChanged verbosity monitor key $ do
-- >       resource <- mkResource
-- >       ... -- use the resource
-- >   | ... ]
--
-- For efficiency one would like to share the resource between the actions
-- but the straightforward way of doing this means initialising it every time
-- even when no actions need re-running.
--
-- > resource <- mkResource
-- > sequence
-- >   [ rerunIfChanged verbosity monitor key $ do
-- >       ... -- use the resource
-- >   | ... ]
--
-- This utility allows one to get the best of both worlds:
--
-- > getResource <- delayInitSharedResource mkResource
-- > sequence
-- >   [ rerunIfChanged verbosity monitor key $ do
-- >       resource <- getResource
-- >       ... -- use the resource
-- >   | ... ]
delayInitSharedResource :: forall a m. MonadIO m => IO a -> m (m a)
delayInitSharedResource action = do
  var <- liftIO (newMVar Nothing)
  return (liftIO (getOrInitResource var))
  where
    getOrInitResource :: MVar (Maybe a) -> IO a
    getOrInitResource var =
      modifyMVar var $ \case
        Just x -> return (Just x, x)
        Nothing -> do
          x <- action
          return (Just x, x)

-- | Much like 'delayInitSharedResource' but for a keyed set of resources.
--
-- > getResource <- delayInitSharedResource mkResource
-- > sequence
-- >   [ rerunIfChanged verbosity monitor key $ do
-- >       resource <- getResource key
-- >       ... -- use the resource
-- >   | ... ]
delayInitSharedResources
  :: forall k v m
   . (Ord k, MonadIO m)
  => (k -> IO v)
  -> m (k -> m v)
delayInitSharedResources action = do
  var <- liftIO (newMVar Map.empty)
  return (liftIO . getOrInitResource var)
  where
    getOrInitResource :: MVar (Map k v) -> k -> IO v
    getOrInitResource var k =
      modifyMVar var $ \m ->
        case Map.lookup k m of
          Just x -> return (m, x)
          Nothing -> do
            x <- action k
            let !m' = Map.insert k x m
            return (m', x)

getDirectoryContentsMonitored :: MonadRebuild m => FilePath -> m [FilePath]
getDirectoryContentsMonitored dir = do
  exists <- monitorDirectoryStatus dir
  if exists
    then liftIO $ listDirectory dir
    else return []

createDirectoryMonitored :: MonadRebuild m => Bool -> FilePath -> m ()
createDirectoryMonitored createParents dir = do
  monitorFiles [monitorDirectoryExistence dir]
  liftIO $ createDirectoryIfMissing createParents dir

-- | Monitor a directory as in 'monitorDirectory' if it currently exists or
-- as 'monitorNonExistentDirectory' if it does not.
monitorDirectoryStatus :: MonadRebuild m => FilePath -> m Bool
monitorDirectoryStatus dir = do
  exists <- liftIO $ doesDirectoryExist dir
  monitorFiles
    [ if exists
        then monitorDirectory dir
        else monitorNonExistentDirectory dir
    ]
  return exists

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | An abstraction for re-running actions if values or files have changed.
--
-- This is not a full-blown make-style incremental build system, it's a bit
-- more ad-hoc than that, but it's easier to integrate with existing code.
--
-- It's a convenient interface to the "Distribution.Client.FileMonitor"
-- functions.
--
module Distribution.Client.RebuildMonad (
    -- * Rebuild monad
    Rebuild,
    runRebuild,

    -- * Setting up file monitoring
    monitorFiles,
    MonitorFilePath(..),
    monitorFileSearchPath,
    FilePathGlob(..),

    -- * Using a file monitor
    FileMonitor(..),
    newFileMonitor,
    rerunIfChanged,

    -- * Utils
    matchFileGlob,
  ) where

import Distribution.Client.FileMonitor

import Distribution.Simple.Utils (debug)
import Distribution.Verbosity    (Verbosity)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.State as State
import Distribution.Compat.Binary     (Binary)
import System.FilePath (takeFileName)


-- | A monad layered on top of 'IO' to help with re-running actions when the
-- input files and values they depend on change. The crucial operations are
-- 'rerunIfChanged' and 'monitorFiles'.
--
newtype Rebuild a = Rebuild (StateT [MonitorFilePath] IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Use this wihin the body action of 'rerunIfChanged' to declare that the
-- action depends on the given files. This can be based on what the action
-- actually did. It is these files that will be checked for changes next
-- time 'rerunIfChanged' is called for that 'FileMonitor'.
--
monitorFiles :: [MonitorFilePath] -> Rebuild ()
monitorFiles filespecs = Rebuild (State.modify (filespecs++))

-- | Run a 'Rebuild' IO action.
unRebuild :: Rebuild a -> IO (a, [MonitorFilePath])
unRebuild (Rebuild action) = runStateT action []

-- | Run a 'Rebuild' IO action.
runRebuild :: Rebuild a -> IO a
runRebuild (Rebuild action) = evalStateT action []

-- | This captures the standard use pattern for a 'FileMonitor': given a
-- monitor, an action and the input value the action depends on, either
-- re-run the action to get its output, or if the value and files the action
-- depends on have not changed then return a previously cached action result.
--
-- The result is still in the 'Rebuild' monad, so these can be nested.
--
-- Do not share 'FileMonitor's between different uses of 'rerunIfChanged'.
--
rerunIfChanged :: (Eq a, Binary a, Binary b)
               => Verbosity
               -> FilePath
               -> FileMonitor a b
               -> a
               -> Rebuild b
               -> Rebuild b
rerunIfChanged verbosity rootDir monitor key action = do
    changed <- liftIO $ checkFileMonitorChanged monitor rootDir key
    case changed of
      MonitorUnchanged result files -> do
        liftIO $ debug verbosity $ "File monitor '" ++ monitorName
                                                    ++ "' unchanged."
        monitorFiles files
        return result

      MonitorChanged reason -> do
        liftIO $ debug verbosity $ "File monitor '" ++ monitorName
                                ++ "' changed: " ++ showReason reason
        startTime <- liftIO $ beginUpdateFileMonitor
        (result, files) <- liftIO $ unRebuild action
        liftIO $ updateFileMonitor monitor rootDir
                                   (Just startTime) files key result
        monitorFiles files
        return result
  where
    monitorName = takeFileName (fileMonitorCacheFile monitor)

    showReason (MonitoredFileChanged file) = "file " ++ file
    showReason (MonitoredValueChanged _)   = "monitor value changed"
    showReason  MonitorFirstRun            = "first run"
    showReason  MonitorCorruptCache        = "invalid cache file"


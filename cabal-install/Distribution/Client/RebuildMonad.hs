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

import Distribution.Client.FileStatusCache
         ( MonitorFilePath(..), monitorFileSearchPath
         , FilePathGlob(..), matchFileGlob
         , FileMonitor(..), newFileMonitor
         , MonitorChanged(..), MonitorChangedReason(..)
         , checkFileMonitorChanged, updateFileMonitor )

import Distribution.Simple.Utils (debug)
import Distribution.Verbosity    (Verbosity)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad.State as State
import Distribution.Compat.Binary     (Binary)
import System.FilePath (takeFileName)


-------------------------------
-- Simple rebuild abstraction
--

newtype Rebuild a = Rebuild (StateT [MonitorFilePath] IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

monitorFiles :: [MonitorFilePath] -> Rebuild ()
monitorFiles filespecs = Rebuild (State.modify (filespecs++))

unRebuild :: Rebuild a -> IO (a, [MonitorFilePath])
unRebuild (Rebuild action) = runStateT action []

runRebuild :: Rebuild a -> IO a
runRebuild (Rebuild action) = evalStateT action []

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
        (result, files) <- liftIO $ unRebuild action
        liftIO $ updateFileMonitor monitor rootDir files key result
        monitorFiles files
        return result
  where
    monitorName = takeFileName (fileMonitorCacheFile monitor)

    showReason (MonitoredFileChanged file) = "file " ++ file
    showReason (MonitoredValueChanged _)   = "monitor value changed"
    showReason  MonitorFirstRun            = "first run"
    showReason  MonitorCorruptCache        = "invalid cache file"


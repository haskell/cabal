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
    FileMonitorCacheFile(..),
    rerunIfChanged,

    -- * Utils
    matchFileGlob,
  ) where

import Distribution.Client.FileStatusCache
         ( MonitorFilePath(..), monitorFileSearchPath
         , FilePathGlob(..), matchFileGlob
         , FileMonitorCacheFile(..), Changed(..)
         , checkFileMonitorChanged, updateFileMonitor )

import Distribution.Simple.Utils (debug)
import Distribution.Verbosity    (Verbosity)

import Control.Applicative
import Control.Monad.State as State
import Data.Maybe      (fromMaybe)
import Data.Binary     (Binary)
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
               -> FileMonitorCacheFile a b
               -> a
               -> Rebuild b
               -> Rebuild b
rerunIfChanged verbosity rootDir monitorCacheFile key action = do
    changed <- liftIO $ checkFileMonitorChanged
                          monitorCacheFile rootDir key
    case changed of
      Unchanged (result, files) -> do
        liftIO $ debug verbosity $ "File monitor '" ++ monitorName
                                                    ++ "' unchanged."
        monitorFiles files
        return result

      Changed mbFile -> do
        liftIO $ debug verbosity $ "File monitor '" ++ monitorName
                                ++ "' changed: "
                                ++ fromMaybe "non-file change" mbFile
        (result, files) <- liftIO $ unRebuild action
        liftIO $ updateFileMonitor monitorCacheFile rootDir
                                   files key result
        monitorFiles files
        return result
  where
    monitorName = takeFileName (monitorCacheFilePath monitorCacheFile)


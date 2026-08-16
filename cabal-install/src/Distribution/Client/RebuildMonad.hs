{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | An abstraction for re-running actions if values or files have changed.
--
-- This is not a full-blown make-style incremental build system, it's a bit
-- more ad-hoc than that, but it's easier to integrate with existing code.
--
-- It's a convenient interface to the "Distribution.Client.FileMonitor"
-- functions.
module Distribution.Client.RebuildMonad
  ( -- * Rebuild monad
    Rebuild
  , runRebuild
  , execRebuild
  , askRoot

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
  , Glob (..)
  , GlobPiece (..)

    -- * Using a file monitor
  , FileMonitor (..)
  , newFileMonitor
  , rerunIfChanged
  , rerunConcurrentlyIfChanged

    -- * Utils
  , delayInitSharedResource
  , delayInitSharedResources
  , matchFileGlob
  , getDirectoryContentsMonitored
  , createDirectoryMonitored
  , monitorDirectoryStatus
  , doesFileExistMonitored
  , need
  , needIfExists
  , findFileWithExtensionMonitored
  , findFirstFileMonitored
  , findFileMonitored
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.RebuildMonad

import Distribution.Client.FileMonitor
import Distribution.Client.Glob hiding (matchFileGlob)
import qualified Distribution.Client.Glob as Glob (matchFileGlob)
import Distribution.Client.JobControl
import Distribution.Simple.PreProcess.Types (Suffix (..))

import Distribution.Simple.Utils (ordNub)

import Control.Monad.Reader as Reader
import Control.Monad.Writer as Writer
import System.Directory
import System.FilePath

-- | A custom rebuild monad to be used for file monitoring within
-- @cabal-install@. On top of the basic 'MonadRebuild' functionality, it keeps
-- the current root path in a 'ReaderT' context.
newtype Rebuild a = Rebuild (ReaderT FilePath (WriterT [MonitorFilePath] IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

deriving newtype instance MonadWriter [MonitorFilePath] Rebuild

instance MonadRebuild Rebuild where
  withRunRebuildInIO action = do
    rootDir <- askRoot
    let runInIO = unRebuild rootDir
    (result, files) <- liftIO $ action runInIO
    Writer.tell files
    return result

-- | Run a 'Rebuild' IO action.
unRebuild :: FilePath -> Rebuild a -> IO (a, [MonitorFilePath])
unRebuild rootDir (Rebuild action) = runWriterT (runReaderT action rootDir)

-- | Run a 'Rebuild' IO action.
runRebuild :: FilePath -> Rebuild a -> IO a
runRebuild rootDir (Rebuild action) = fst <$> runWriterT (runReaderT action rootDir)

-- | Run a 'Rebuild' IO action.
execRebuild :: FilePath -> Rebuild a -> IO [MonitorFilePath]
execRebuild rootDir (Rebuild action) = execWriterT (runReaderT action rootDir)

-- | The root that relative paths are interpreted as being relative to.
askRoot :: Rebuild FilePath
askRoot = Rebuild Reader.ask

-- | This captures the standard use pattern for a 'FileMonitor': given a
-- monitor, an action and the input value the action depends on, either
-- re-run the action to get its output, or if the value and files the action
-- depends on have not changed then return a previously cached action result.
--
-- The result is still in the 'Rebuild' monad, so these can be nested.
--
-- See also 'rerunIfChanged''.
--
-- Do not share 'FileMonitor's between different uses of 'rerunIfChanged'.
rerunIfChanged
  :: (Binary a, Structured a, Binary b, Structured b)
  => Verbosity
  -> FileMonitor a b
  -> a
  -> Rebuild b
  -> Rebuild b
rerunIfChanged verbosity monitor key action = do
  -- rerunIfChanged is implemented in terms of rerunConcurrentlyIfChanged, but
  -- nothing concurrent will happen since the list of concurrent actions has a
  -- single value that will be waited for alone.
  rerunConcurrentlyIfChanged verbosity newSerialJobControl [(monitor, key, action)] >>= \case
    [x] -> return x
    _ -> error "rerunIfChanged: impossible!"

-- | 'rerunIfChanged' meets 'mapConcurrently': For when we want multiple actions
-- that need to do be re-run-if-changed asynchronously. The function returns
-- when all values have finished computing.
rerunConcurrentlyIfChanged
  :: (Binary a, Structured a, Binary b, Structured b)
  => Verbosity
  -> IO (JobControl IO (b, [MonitorFilePath]))
  -> [(FileMonitor a b, a, Rebuild b)]
  -> Rebuild [b]
rerunConcurrentlyIfChanged verbosity mkJobControl triples = do
  rootDir <- askRoot
  rerunIfChanged' verbosity rootDir chainIOs triples
  where
    chainIOs dacts =
      withJobControl mkJobControl $ \jobControl ->
        mapConcurrentWithJobs jobControl id dacts

-- | Utility to match a file glob against the file system, starting from a
-- given root directory. The results are all relative to the given root.
--
-- Since this operates in the 'Rebuild' monad, it also monitors the given glob
-- for changes.
matchFileGlob :: RootedGlob -> Rebuild [FilePath]
matchFileGlob glob = do
  root <- askRoot
  monitorFiles [monitorFileGlobExistence glob]
  liftIO $ Glob.matchFileGlob root glob

-- | Like 'doesFileExist', but in the 'Rebuild' monad.  This does
-- NOT track the contents of 'FilePath'; use 'need' in that case.
doesFileExistMonitored :: FilePath -> Rebuild Bool
doesFileExistMonitored f = do
  root <- askRoot
  exists <- liftIO $ doesFileExist (root </> f)
  monitorFiles
    [ if exists
        then monitorFileExistence f
        else monitorNonExistentFile f
    ]
  return exists

-- | Monitor a single file
need :: FilePath -> Rebuild ()
need f = monitorFiles [monitorFileHashed f]

-- | Monitor a file if it exists; otherwise check for when it
-- gets created.  This is a bit better for recompilation avoidance
-- because sometimes users give bad package metadata, and we don't
-- want to repeatedly rebuild in this case (which we would if we
-- need'ed a non-existent file).
needIfExists :: FilePath -> Rebuild ()
needIfExists f = do
  root <- askRoot
  exists <- liftIO $ doesFileExist (root </> f)
  monitorFiles
    [ if exists
        then monitorFileHashed f
        else monitorNonExistentFile f
    ]

-- | Like 'findFileWithExtension', but in the 'Rebuild' monad.
findFileWithExtensionMonitored
  :: [Suffix]
  -> [FilePath]
  -> FilePath
  -> Rebuild (Maybe FilePath)
findFileWithExtensionMonitored extensions searchPath baseName =
  findFirstFileMonitored
    id
    [ path </> baseName <.> ext
    | path <- ordNub searchPath
    , Suffix ext <- ordNub extensions
    ]

-- | Like 'findFirstFile', but in the 'Rebuild' monad.
findFirstFileMonitored :: forall a. (a -> FilePath) -> [a] -> Rebuild (Maybe a)
findFirstFileMonitored file = findFirst
  where
    findFirst :: [a] -> Rebuild (Maybe a)
    findFirst [] = return Nothing
    findFirst (x : xs) = do
      exists <- doesFileExistMonitored (file x)
      if exists
        then return (Just x)
        else findFirst xs

-- | Like 'findFile', but in the 'Rebuild' monad.
findFileMonitored :: [FilePath] -> FilePath -> Rebuild (Maybe FilePath)
findFileMonitored searchPath fileName =
  findFirstFileMonitored
    id
    [ path </> fileName
    | path <- ordNub searchPath
    ]

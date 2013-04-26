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
  withTimestamps
  ) where

import Data.Char                      ( isSpace )
import System.Directory               ( renameFile )
import System.FilePath                ( (<.>), (</>) )

import Distribution.Simple.PreProcess ( PPSuffixHandler, knownSuffixHandlers,
                                        ppSuffixes )
import Distribution.PackageDescription.Configuration
                                      ( flattenPackageDescription )
import Distribution.PackageDescription.Parse
                                      ( readPackageDescription )
import Distribution.Simple.Utils      ( die, findPackageDesc )

import Distribution.Compat.Exception  ( catchIO )
import Distribution.Compat.Time       ( EpochTime, getCurTime, getModTime )


-- | Timestamp of an add-source dependency.
type AddSourceTimestamp = (FilePath, EpochTime)

-- | The 'add-source-timestamps' file keeps the timestamps of all add-source
-- dependencies. It is initially populated by 'sandbox add-source' and kept
-- current by 'installAddSourceDeps'. The user can install add-source deps
-- manually with 'cabal install' after having edited them, so we can err on the
-- side of caution sometimes.
timestampFileName :: FilePath
timestampFileName = "add-source-timestamps"

-- | Read the timestamp file. Returns an empty list if the file doesn't exist.
readTimestamps :: FilePath -> IO (Maybe [AddSourceTimestamp])
readTimestamps sandboxDir = do
  timestampString <- readFile timestampFile `catchIO` \_ -> return "[]"
  case reads timestampString of
    [(timestamps, s)] | all isSpace s -> return (Just timestamps)
    _                                 -> return Nothing
  where
    timestampFile = sandboxDir </> timestampFileName

-- | Write the timestamp file, atomically.
writeTimestamps :: FilePath -> [AddSourceTimestamp] -> IO ()
writeTimestamps sandboxDir timestamps = do
  writeFile  timestampTmpFile (show timestamps)
  renameFile timestampTmpFile timestampFile
  where
    timestampFile    = sandboxDir </> timestampFileName
    timestampTmpFile = timestampFile <.> "tmp"

-- | Given a list of 'AddSourceTimestamp's, a list of paths to add-source deps
-- we've reinstalled and a new timestamp value, update the timestamp value for
-- those deps.
updateTimestamps :: [AddSourceTimestamp] -> [FilePath] -> EpochTime
                    -> [AddSourceTimestamp]
updateTimestamps l pathsToUpdate newTimestamp = foldr updateTimestamp [] l
  where
    updateTimestamp t@(path, _oldTimestamp) rest =
      if path `elem` pathsToUpdate
      then (path, newTimestamp) : rest
      else t : rest

-- | Given an IO action, feed to it the timestamps from the timestamp file, and
-- then update the timestamps of the returned build tree refs to the current
-- time.
withTimestamps :: FilePath -> ([AddSourceTimestamp] -> IO ([FilePath]))
                  -> IO ()
withTimestamps sandboxDir act = do
  mTimestamps <- readTimestamps sandboxDir
  case mTimestamps of
    Nothing         -> die $ "The timestamps file is corrupted. "
                       ++ "Please delete & recreate the sandbox."
    Just timestamps -> do
      updatedPaths <- act timestamps
      now <- getCurTime
      let timestamps' = updateTimestamps timestamps updatedPaths now
      writeTimestamps sandboxDir timestamps'

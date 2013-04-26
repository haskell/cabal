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
  withRemoveTimestamps,
  withUpdateTimestamps,
  isDepModified,
  withModifiedDeps,
  ) where

import Control.Monad                                 (filterM, forM)
import Data.Char                                     (isSpace)
import Data.Maybe                                    (maybeToList)
import Data.List                                     (partition)
import System.Directory                              (renameFile)
import System.FilePath                               (isAbsolute, (<.>), (</>))

import Distribution.PackageDescription               (BuildInfo (..),
                                                      Executable (..),
                                                      Library (..),
                                                      PackageDescription (..))
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse         (readPackageDescription)
import Distribution.Simple.PreProcess                (knownSuffixHandlers)
import Distribution.Simple.SrcDist                   (allSourcesBuildInfo,
                                                      filterAutogenModule,
                                                      findIncludeFile,
                                                      findMainExeFile,
                                                      findSetupFile)
import Distribution.Simple.Utils                     (defaultPackageDesc, die,
                                                      findPackageDesc,
                                                      matchFileGlob)
import Distribution.Verbosity                        (Verbosity)

import Distribution.Client.Utils                     (inDir)

import Distribution.Compat.Exception                 (catchIO)
import Distribution.Compat.Time                      (EpochTime, getCurTime,
                                                      getModTime)


-- | Timestamp of an add-source dependency.
type AddSourceTimestamp = (FilePath, EpochTime)

-- | The 'add-source-timestamps' file keeps the timestamps of all add-source
-- dependencies. It is initially populated by 'sandbox add-source' and kept
-- current by 'reinstallAddSourceDeps'. The user can install add-source deps
-- manually with 'cabal install' after having edited them, so we can err on the
-- side of caution sometimes.
-- FIXME: We should keep this info in the index file, together with build tree
-- refs.
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
-- those deps. If there are new paths in the list, add them to the timestamp
-- file with the current date.
updateTimestamps :: EpochTime -> [AddSourceTimestamp] -> [FilePath]
                    -> [AddSourceTimestamp]
updateTimestamps newTimestamp timestamps paths =
  map (\p -> (p, newTimestamp)) newPaths ++ foldr updateTimestamp [] timestamps
  where
    oldPaths = map fst timestamps
    (pathsToUpdate, newPaths) = partition (flip elem oldPaths) paths

    updateTimestamp t@(path, _oldTimestamp) rest
      | path `elem` pathsToUpdate = (path, newTimestamp) : rest
      | otherwise                 = t : rest

-- | Given a list of 'AddSourceTimestamp's and a list of paths to add-source
-- deps we've removed, remove those deps from the list.
removeTimestamps :: [AddSourceTimestamp] -> [FilePath] -> [AddSourceTimestamp]
removeTimestamps l pathsToRemove = foldr removeTimestamp [] l
  where
    removeTimestamp t@(path, _oldTimestamp) rest =
      if path `elem` pathsToRemove
      then rest
      else t : rest

-- | Given an IO action that returns a list of build tree refs, remove those
-- build tree refs to the current time.
withRemoveTimestamps :: FilePath -> ([AddSourceTimestamp] -> IO [FilePath])
                        -> IO ()
withRemoveTimestamps = withActionOnTimestamps removeTimestamps

-- | Given an IO action that returns a list of build tree refs, update the
-- timestamps of the returned build tree refs to the current time.
withUpdateTimestamps :: FilePath -> ([AddSourceTimestamp] -> IO [FilePath])
                        -> IO ()
withUpdateTimestamps sandboxDir act = do
  now <- getCurTime
  withActionOnTimestamps (updateTimestamps now) sandboxDir act

-- | Helper for implementing 'withUpdateTimestamps' and 'withRemoveTimestamps'.
withActionOnTimestamps :: ([AddSourceTimestamp] -> [FilePath]
                           -> [AddSourceTimestamp])
                          -> FilePath
                          -> ([AddSourceTimestamp] -> IO [FilePath])
                          -> IO ()
withActionOnTimestamps f sandboxDir act = do
  mTimestamps <- readTimestamps sandboxDir
  case mTimestamps of
    Nothing         -> die $ "The timestamps file is corrupted. "
                       ++ "Please delete & recreate the sandbox."
    Just timestamps -> do
      updatedPaths <- act timestamps
      let timestamps' = f timestamps updatedPaths
      writeTimestamps sandboxDir timestamps'

-- | List all source files of a given add-source dependency. Exits with error if
-- something is wrong (e.g. there is no .cabal file in the given directory).
-- FIXME: This function is not thread-safe because of 'inDir'.
allPackageSourceFiles :: Verbosity -> FilePath -> IO [FilePath]
allPackageSourceFiles verbosity packageDir = inDir (Just packageDir) $ do
  pkgDesc <- fmap (filterAutogenModule . flattenPackageDescription)
             . readPackageDescription verbosity =<< findPackageDesc packageDir
  -- NOTE: This is patterned after "Distribution.Simple.SrcDist.prepareTree".
  libSources <- withLib pkgDesc $
                \Library { exposedModules = modules, libBuildInfo = libBi } ->
                allSourcesBuildInfo libBi pps modules
  exeSources <- withExe pkgDesc $
                \Executable { modulePath = mainPath, buildInfo = exeBi } -> do
                biSrcs  <- allSourcesBuildInfo exeBi pps []
                mainSrc <- findMainExeFile exeBi pps mainPath
                return (mainSrc:biSrcs)

  -- We don't care about test and benchmark sources.

  dataFs    <- forM (dataFiles pkgDesc) $ \filename ->
    matchFileGlob (dataDir pkgDesc </> filename)

  extraSrcs <- forM (extraSrcFiles pkgDesc) $ \fpath ->
    matchFileGlob fpath

  incFiles  <- withLib pkgDesc $ \ l -> do
    let lbi = libBuildInfo l
        relincdirs = "." : filter (not.isAbsolute) (includeDirs lbi)
    mapM (fmap snd . findIncludeFile relincdirs) (installIncludes lbi)

  mSetupFile <- findSetupFile
  descFile   <- defaultPackageDesc verbosity

  return . map (packageDir </>) $ descFile : (maybeToList mSetupFile)
    ++ incFiles ++ (concat extraSrcs) ++ (concat dataFs)
    ++ (concat exeSources) ++ libSources

  where
    -- We have to deal with all libs and executables, so we have local
    -- versions of these functions that ignore the 'buildable' attribute:
    withLib pkgDesc action = maybe (return []) action (library pkgDesc)
    withExe pkgDesc action = mapM action (executables pkgDesc)

    pps = knownSuffixHandlers


-- | Has this dependency been modified since we have last looked at it?
isDepModified :: Verbosity -> AddSourceTimestamp -> IO Bool
isDepModified verbosity (packageDir, timestamp) = do
  depSources <- allPackageSourceFiles verbosity packageDir
  go depSources

  where
    go []         = return False
    go (dep:rest) = do modTime <- getModTime dep
                       if modTime >= timestamp
                         then return True
                         else go rest

-- | Given an IO action, feed to it the list of modified add-source deps and
-- set their timestamps to the current time in the timestamps file.
withModifiedDeps :: Verbosity -> FilePath -> ([FilePath] -> IO ()) -> IO ()
withModifiedDeps verbosity sandboxDir act = do
  withUpdateTimestamps sandboxDir $ \timestamps -> do
    modified <- fmap (map fst) . filterM (isDepModified verbosity) $ timestamps
    act modified
    return modified

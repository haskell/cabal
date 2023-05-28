{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.CmdClean (cleanCommand, cleanAction) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Config
  ( defaultScriptBuildsDir
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , defaultDistDirLayout
  )
import Distribution.Client.ProjectConfig
  ( findProjectRoot
  )
import Distribution.Client.Setup
  ( GlobalFlags
  )
import Distribution.ReadE (succeedReadE)
import Distribution.Simple.Command
  ( CommandUI (..)
  , option
  , reqArg
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , falseArg
  , flagToList
  , flagToMaybe
  , fromFlagOrDefault
  , optionDistPref
  , optionVerbosity
  , toFlag
  )
import Distribution.Simple.Utils
  ( die'
  , handleDoesNotExist
  , info
  , wrapText
  )
import Distribution.Verbosity
  ( normal
  )

import Control.Monad
  ( forM
  , forM_
  , mapM
  )
import qualified Data.Set as Set
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath
  ( (</>)
  )

data CleanFlags = CleanFlags
  { cleanSaveConfig :: Flag Bool
  , cleanVerbosity :: Flag Verbosity
  , cleanDistDir :: Flag FilePath
  , cleanProjectDir :: Flag FilePath
  , cleanProjectFile :: Flag FilePath
  }
  deriving (Eq)

defaultCleanFlags :: CleanFlags
defaultCleanFlags =
  CleanFlags
    { cleanSaveConfig = toFlag False
    , cleanVerbosity = toFlag normal
    , cleanDistDir = NoFlag
    , cleanProjectDir = mempty
    , cleanProjectFile = mempty
    }

cleanCommand :: CommandUI CleanFlags
cleanCommand =
  CommandUI
    { commandName = "v2-clean"
    , commandSynopsis = "Clean the package store and remove temporary files."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " new-clean [FLAGS]\n"
    , commandDescription = Just $ \_ ->
        wrapText $
          "Removes all temporary files created during the building process "
            ++ "(.hi, .o, preprocessed sources, etc.) and also empties out the "
            ++ "local caches (by default).\n\n"
    , commandNotes = Nothing
    , commandDefaultFlags = defaultCleanFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity
            cleanVerbosity
            (\v flags -> flags{cleanVerbosity = v})
        , optionDistPref
            cleanDistDir
            (\dd flags -> flags{cleanDistDir = dd})
            showOrParseArgs
        , option
            []
            ["project-dir"]
            "Set the path of the project directory"
            cleanProjectDir
            (\path flags -> flags{cleanProjectDir = path})
            (reqArg "DIR" (succeedReadE Flag) flagToList)
        , option
            []
            ["project-file"]
            "Set the path of the cabal.project file (relative to the project directory when relative)"
            cleanProjectFile
            (\pf flags -> flags{cleanProjectFile = pf})
            (reqArg "FILE" (succeedReadE Flag) flagToList)
        , option
            ['s']
            ["save-config"]
            "Save configuration, only remove build artifacts"
            cleanSaveConfig
            (\sc flags -> flags{cleanSaveConfig = sc})
            falseArg
        ]
    }

cleanAction :: CleanFlags -> [String] -> GlobalFlags -> IO ()
cleanAction CleanFlags{..} extraArgs _ = do
  let verbosity = fromFlagOrDefault normal cleanVerbosity
      saveConfig = fromFlagOrDefault False cleanSaveConfig
      mdistDirectory = flagToMaybe cleanDistDir
      mprojectDir = flagToMaybe cleanProjectDir
      mprojectFile = flagToMaybe cleanProjectFile

  -- TODO interpret extraArgs as targets and clean those targets only (issue #7506)
  --
  -- For now assume all files passed are the names of scripts
  notScripts <- filterM (fmap not . doesFileExist) extraArgs
  unless (null notScripts) $
    die' verbosity $
      "'clean' extra arguments should be script files: "
        ++ unwords notScripts

  projectRoot <- either throwIO return =<< findProjectRoot verbosity mprojectDir mprojectFile

  let distLayout = defaultDistDirLayout projectRoot mdistDirectory Nothing

  -- Do not clean a project if just running a script in it's directory
  when (null extraArgs || isJust mdistDirectory) $ do
    if saveConfig
      then do
        let buildRoot = distBuildRootDirectory distLayout

        buildRootExists <- doesDirectoryExist buildRoot

        when buildRootExists $ do
          info verbosity ("Deleting build root (" ++ buildRoot ++ ")")
          handleDoesNotExist () $ removeDirectoryRecursive buildRoot
      else do
        let distRoot = distDirectory distLayout

        info verbosity ("Deleting dist-newstyle (" ++ distRoot ++ ")")
        handleDoesNotExist () $ removeDirectoryRecursive distRoot

    removeEnvFiles (distProjectRootDirectory distLayout)

  -- Clean specified script build caches and orphaned caches.
  -- There is currently no good way to specify to only clean orphaned caches.
  -- It would be better as part of an explicit gc step (see issue #3333)
  toClean <- Set.fromList <$> mapM canonicalizePath extraArgs
  cacheDir <- defaultScriptBuildsDir
  existsCD <- doesDirectoryExist cacheDir
  caches <- if existsCD then listDirectory cacheDir else return []
  paths <- fmap concat . forM caches $ \cache -> do
    let locFile = cacheDir </> cache </> "scriptlocation"
    exists <- doesFileExist locFile
    if exists then pure . (,) (cacheDir </> cache) <$> readFile locFile else return []
  forM_ paths $ \(cache, script) -> do
    exists <- doesFileExist script
    when (not exists || script `Set.member` toClean) $ do
      info verbosity ("Deleting cache (" ++ cache ++ ") for script (" ++ script ++ ")")
      removeDirectoryRecursive cache

removeEnvFiles :: FilePath -> IO ()
removeEnvFiles dir =
  (traverse_ (removeFile . (dir </>)) . filter ((".ghc.environment" ==) . take 16))
    =<< getDirectoryContents dir

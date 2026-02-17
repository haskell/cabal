{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.CmdClean (cleanCommand, cleanAction) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Config
  ( defaultScriptBuildsDir
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , ProjectRoot (ProjectRootImplicit)
  , defaultDistDirLayout
  , defaultProjectFile
  )
import Distribution.Client.Errors
import Distribution.Client.ProjectConfig
  ( findProjectRoot
  )
import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  , removeIgnoreProjectOption
  )
import Distribution.Client.Setup
  ( GlobalFlags
  )
import Distribution.Compat.Lens
  ( _1
  , _2
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , liftOptionL
  , option
  )
import Distribution.Simple.Setup
  ( Flag
  , falseArg
  , flagToMaybe
  , fromFlagOrDefault
  , optionDistPref
  , optionVerbosity
  , toFlag
  , pattern NoFlag
  )
import Distribution.Simple.Utils
  ( dieWithException
  , handleDoesNotExist
  , info
  , wrapText
  )
import Distribution.System
  ( OS (Windows)
  , buildOS
  )
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )
import Distribution.Verbosity
  ( VerbosityFlags
  , defaultVerbosityHandles
  , mkVerbosity
  , normal
  )

import Control.Exception
  ( throw
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
  , removePathForcibly
  )
import System.FilePath
  ( (</>)
  )
import System.IO.Error
  ( isPermissionError
  )
import qualified System.Process as Process

data CleanFlags = CleanFlags
  { cleanSaveConfig :: Flag Bool
  , cleanVerbosity :: Flag VerbosityFlags
  , cleanDistDir :: Flag (SymbolicPath Pkg (Dir Dist))
  }
  deriving (Eq)

defaultCleanFlags :: CleanFlags
defaultCleanFlags =
  CleanFlags
    { cleanSaveConfig = toFlag False
    , cleanVerbosity = toFlag normal
    , cleanDistDir = NoFlag
    }

cleanCommand :: CommandUI (ProjectFlags, CleanFlags)
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
    , commandDefaultFlags = (defaultProjectFlags, defaultCleanFlags)
    , commandOptions = \showOrParseArgs ->
        map
          (liftOptionL _1)
          (removeIgnoreProjectOption (projectFlagsOptions showOrParseArgs))
          ++ map (liftOptionL _2) (cleanOptions showOrParseArgs)
    }

cleanOptions :: ShowOrParseArgs -> [OptionField CleanFlags]
cleanOptions showOrParseArgs =
  [ optionVerbosity
      cleanVerbosity
      (\v flags -> flags{cleanVerbosity = v})
  , optionDistPref
      cleanDistDir
      (\dd flags -> flags{cleanDistDir = dd})
      showOrParseArgs
  , option
      ['s']
      ["save-config"]
      "Save configuration, only remove build artifacts"
      cleanSaveConfig
      (\sc flags -> flags{cleanSaveConfig = sc})
      falseArg
  ]

cleanAction :: (ProjectFlags, CleanFlags) -> [String] -> GlobalFlags -> IO ()
cleanAction (ProjectFlags{..}, CleanFlags{..}) extraArgs _ = do
  let verbosity = mkVerbosity defaultVerbosityHandles $ fromFlagOrDefault normal cleanVerbosity
      saveConfig = fromFlagOrDefault False cleanSaveConfig
      mdistDirectory = fmap getSymbolicPath $ flagToMaybe cleanDistDir
      mprojectDir = flagToMaybe flagProjectDir
      mprojectFile = flagToMaybe flagProjectFile

  -- TODO interpret extraArgs as targets and clean those targets only (issue #7506)
  --
  -- For now assume all files passed are the names of scripts
  notScripts <- filterM (fmap not . doesFileExist) extraArgs
  unless (null notScripts) $
    dieWithException verbosity $
      CleanActionNotScript notScripts

  projectRoot <- either throwIO return =<< findProjectRoot verbosity mprojectDir mprojectFile

  let distLayout = defaultDistDirLayout projectRoot mdistDirectory Nothing

  -- Do not clean a project if just running a script in it's directory
  when (null extraArgs || isJust mdistDirectory) $ do
    isValid <- isValidProjectRoot projectRoot
    unless isValid $ dieWithException verbosity CleanActionNotPackage

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
        handleDoesNotExist () $ do
          if buildOS == Windows
            then do
              -- Windows can't delete some git files #10182
              void $
                Process.createProcess_ "attrib" $
                  Process.shell $
                    "attrib -s -h -r " <> distRoot <> "\\*.* /s /d"
              catch
                (removePathForcibly distRoot)
                (\e -> if isPermissionError e then removePathForcibly distRoot else throw e)
            else removeDirectoryRecursive distRoot

    removeEnvFiles $ distProjectRootDirectory distLayout

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

isValidProjectRoot :: ProjectRoot -> IO Bool
isValidProjectRoot = \case
  (ProjectRootImplicit dir) -> do
    let projectFile = dir </> defaultProjectFile
    projectExists <- doesFileExist projectFile
    contents <- listDirectory dir
    let cabalFiles = filter (".cabal" `isSuffixOf`) contents
    pure (projectExists || not (null cabalFiles))
  _ -> pure True

removeEnvFiles :: FilePath -> IO ()
removeEnvFiles dir =
  (traverse_ (removeFile . (dir </>)) . filter ((".ghc.environment" ==) . take 16))
    =<< getDirectoryContents dir

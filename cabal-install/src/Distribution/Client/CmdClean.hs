{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.CmdClean (cleanCommand, cleanAction) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.DistDirLayout
    ( DistDirLayout(..), defaultDistDirLayout )
import Distribution.Client.NixStyleOptions
    ( NixStyleFlags (..), defaultNixStyleFlags )
import Distribution.Client.ProjectConfig
    ( findProjectRoot )
import Distribution.Client.ProjectFlags
    ( ProjectFlags(..), projectFlagsOptions )
import Distribution.Client.ScriptUtils
    ( getScriptCacheDirectoryRoot )
import Distribution.Client.Setup
    ( ConfigFlags(..), GlobalFlags, liftOptions )
import Distribution.Simple.Setup
    ( Flag(..), toFlag, fromFlagOrDefault, flagToMaybe
    , optionDistPref, optionVerbosity, falseArg
    )
import Distribution.Simple.Command
    ( CommandUI(..), option )
import Distribution.Simple.Utils
    ( info, die', wrapText, handleDoesNotExist )
import Distribution.Verbosity
    ( normal )

import Control.Monad
    ( forM, forM_, mapM )
import qualified Data.Set as Set
import System.Directory
    ( removeDirectoryRecursive, removeFile
    , doesDirectoryExist, doesFileExist
    , getDirectoryContents, listDirectory
    , canonicalizePath )
import System.FilePath
    ( (</>) )

data CleanFlags = CleanFlags
    { cleanSaveConfig :: Flag Bool
    } deriving (Eq)

defaultCleanFlags :: CleanFlags
defaultCleanFlags = CleanFlags
    { cleanSaveConfig = toFlag False
    }

cleanCommand :: CommandUI (NixStyleFlags CleanFlags)
cleanCommand = CommandUI
    { commandName         = "v2-clean"
    , commandSynopsis     = "Clean the package store and remove temporary files."
    , commandUsage        = \pname ->
        "Usage: " ++ pname ++ " new-clean [FLAGS]\n"
    , commandDescription  = Just $ \_ -> wrapText $
        "Removes all temporary files created during the building process "
     ++ "(.hi, .o, preprocessed sources, etc.) and also empties out the "
     ++ "local caches (by default).\n\n"
    , commandNotes        = Nothing
    , commandDefaultFlags = defaultNixStyleFlags defaultCleanFlags
    , commandOptions      = \showOrParseArgs ->
        [ optionVerbosity
            (configVerbosity . configFlags)
            (\v flags -> flags { configFlags = (configFlags flags) { configVerbosity = v } })
        , optionDistPref
            (configDistPref . configFlags)
            (\dd flags -> flags { configFlags = (configFlags flags) { configDistPref = dd } })
            showOrParseArgs
        ] ++ liftOptions projectFlags
               (\x flags -> flags { projectFlags = x })
               (projectFlagsOptions showOrParseArgs)
          ++
        [ option ['s'] ["save-config"]
            "Save configuration, only remove build artifacts"
            (cleanSaveConfig . extraFlags)
            (\sc flags -> flags { extraFlags = (extraFlags flags) { cleanSaveConfig = sc } })
            falseArg
        ]
  }

cleanAction :: NixStyleFlags CleanFlags -> [String] -> GlobalFlags -> IO ()
cleanAction NixStyleFlags{..} extraArgs _ = do
    let verbosity      = fromFlagOrDefault normal $ configVerbosity configFlags
        saveConfig     = fromFlagOrDefault False  $ cleanSaveConfig extraFlags
        mdistDirectory = flagToMaybe $ configDistPref configFlags
        mprojectFile   = flagToMaybe $ flagProjectFileName projectFlags

    -- TODO interpret extraArgs as targets and clean those targets only (issue #7506)
    --
    -- For now assume all files passed are the names of scripts
    notScripts <- filterM (fmap not . doesFileExist) extraArgs
    unless (null notScripts) $
        die' verbosity $ "'clean' extra arguments should be script files: "
                         ++ unwords notScripts

    projectRoot <- either throwIO return =<< findProjectRoot Nothing mprojectFile

    let distLayout = defaultDistDirLayout projectRoot mdistDirectory

    -- Do not clean a project if just running a script in it's directory
    when (null extraArgs || isJust mdistDirectory) $ do
        if saveConfig then do
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
    toClean  <- Set.fromList <$> mapM canonicalizePath extraArgs
    cacheDir <- getScriptCacheDirectoryRoot
    existsCD <- doesDirectoryExist cacheDir
    caches   <- if existsCD then listDirectory cacheDir else return []
    paths    <- fmap concat . forM caches $ \cache -> do
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

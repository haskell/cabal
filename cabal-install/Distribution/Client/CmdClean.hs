{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.CmdClean (cleanCommand, cleanAction) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.DistDirLayout
    ( DistDirLayout(..), defaultDistDirLayout )
import Distribution.Client.ProjectConfig
    ( findProjectRoot )
import Distribution.Client.Setup
    ( GlobalFlags )
import Distribution.ReadE ( succeedReadE )
import Distribution.Simple.Setup
    ( Flag(..), toFlag, fromFlagOrDefault, flagToList, flagToMaybe
    , optionDistPref, optionVerbosity, falseArg
    )
import Distribution.Simple.Command
    ( CommandUI(..), option, reqArg )
import Distribution.Simple.Utils
    ( info, die', wrapText, handleDoesNotExist )
import Distribution.Verbosity
    ( Verbosity, normal )

import Control.Exception
    ( throwIO )
import System.Directory
    ( removeDirectoryRecursive, doesDirectoryExist )

data CleanFlags = CleanFlags
    { cleanSaveConfig :: Flag Bool
    , cleanVerbosity :: Flag Verbosity
    , cleanDistDir :: Flag FilePath
    , cleanProjectFile :: Flag FilePath
    } deriving (Eq)

defaultCleanFlags :: CleanFlags
defaultCleanFlags = CleanFlags
    { cleanSaveConfig = toFlag False
    , cleanVerbosity = toFlag normal
    , cleanDistDir = NoFlag
    , cleanProjectFile = mempty
    }

cleanCommand :: CommandUI CleanFlags
cleanCommand = CommandUI
    { commandName = "new-clean"
    , commandSynopsis = "Clean the package store and remove temporary files."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " new-clean [FLAGS]\n"
    , commandDescription  = Just $ \_ -> wrapText $
        "Removes all temporary files created during the building process "
     ++ "(.hi, .o, preprocessed sources, etc.) and also empties out the "
     ++ "local caches (by default).\n\n"
    , commandNotes = Nothing
    , commandDefaultFlags = defaultCleanFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity
            cleanVerbosity (\v flags -> flags { cleanVerbosity = v })
        , optionDistPref
            cleanDistDir (\dd flags -> flags { cleanDistDir = dd })
            showOrParseArgs
        , option [] ["project-file"]
            "Set the name of the cabal.project file to search for in parent directories"
            cleanProjectFile (\pf flags -> flags {cleanProjectFile = pf})
            (reqArg "FILE" (succeedReadE Flag) flagToList)
        , option ['s'] ["save-config"]
            "Save configuration, only remove build artifacts"
            cleanSaveConfig (\sc flags -> flags { cleanSaveConfig = sc })
            falseArg
        ]
  }

cleanAction :: CleanFlags -> [String] -> GlobalFlags -> IO ()
cleanAction CleanFlags{..} extraArgs _ = do
    let verbosity = fromFlagOrDefault normal cleanVerbosity
        saveConfig = fromFlagOrDefault False cleanSaveConfig
        mdistDirectory = flagToMaybe cleanDistDir
        mprojectFile = flagToMaybe cleanProjectFile

    unless (null extraArgs) $
        die' verbosity $ "'clean' doesn't take any extra arguments: " ++ unwords extraArgs

    projectRoot <- either throwIO return =<< findProjectRoot Nothing mprojectFile

    let distLayout = defaultDistDirLayout projectRoot mdistDirectory

    if saveConfig
        then do
            let buildRoot = distBuildRootDirectory distLayout
                unpackedSrcRoot = distUnpackedSrcRootDirectory distLayout

            buildRootExists <- doesDirectoryExist buildRoot
            unpackedSrcRootExists <- doesDirectoryExist unpackedSrcRoot

            when buildRootExists $ do
                info verbosity ("Deleting build root (" ++ buildRoot ++ ")")
                handleDoesNotExist () $ removeDirectoryRecursive buildRoot

            when unpackedSrcRootExists $ do
                info verbosity ("Deleting unpacked source root (" ++ unpackedSrcRoot ++ ")")
                handleDoesNotExist () $ removeDirectoryRecursive unpackedSrcRoot
        else do
            let distRoot = distDirectory distLayout

            info verbosity ("Deleting dist-newstyle (" ++ distRoot ++ ")")
            handleDoesNotExist () $ removeDirectoryRecursive distRoot

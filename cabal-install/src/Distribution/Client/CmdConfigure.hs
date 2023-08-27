{-# LANGUAGE RecordWildCards #-}

-- | cabal-install CLI command: configure
module Distribution.Client.CmdConfigure
  ( configureCommand
  , configureAction
  , configureAction'
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory
import System.FilePath

import Distribution.Client.ProjectConfig
  ( readProjectLocalExtraConfig
  , writeProjectLocalExtraConfig
  )
import Distribution.Client.ProjectFlags
  ( removeIgnoreProjectOption
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Simple.Flag

import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.Setup
  ( ConfigExFlags (..)
  , ConfigFlags (..)
  , GlobalFlags
  )
import Distribution.Verbosity
  ( normal
  )

import Distribution.Simple.Command
  ( CommandUI (..)
  , usageAlternatives
  )
import Distribution.Simple.Utils
  ( dieWithException
  , notice
  , wrapText
  )

import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  )
import Distribution.Client.Errors
import Distribution.Client.HttpUtils
import Distribution.Client.ProjectConfig.Types
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Types.CondTree
  ( CondTree (..)
  )
import Distribution.Utils.NubList
  ( fromNubList
  )

configureCommand :: CommandUI (NixStyleFlags ())
configureCommand =
  CommandUI
    { commandName = "v2-configure"
    , commandSynopsis = "Add extra project configuration."
    , commandUsage = usageAlternatives "v2-configure" ["[FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Adjust how the project is built by setting additional package flags "
            ++ "and other flags.\n\n"
            ++ "The configuration options are written to the 'cabal.project.local' "
            ++ "file (or '$project_file.local', if '--project-file' is specified) "
            ++ "which extends the configuration from the 'cabal.project' file "
            ++ "(if any). This combination is used as the project configuration for "
            ++ "all other commands (such as 'v2-build', 'v2-repl' etc) though it "
            ++ "can be extended/overridden on a per-command basis.\n\n"
            ++ "The v2-configure command also checks that the project configuration "
            ++ "will work. In particular it checks that there is a consistent set of "
            ++ "dependencies for the project as a whole.\n\n"
            ++ "The 'cabal.project.local' file persists across 'v2-clean' but is "
            ++ "overwritten on the next use of the 'v2-configure' command. The "
            ++ "intention is that the 'cabal.project' file should be kept in source "
            ++ "control but the 'cabal.project.local' should not.\n\n"
            ++ "It is never necessary to use the 'v2-configure' command. It is "
            ++ "merely a convenience in cases where you do not want to specify flags "
            ++ "to 'v2-build' (and other commands) every time and yet do not want "
            ++ "to alter the 'cabal.project' persistently."
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-configure --with-compiler ghc-7.10.3\n"
          ++ "    Adjust the project configuration to use the given compiler\n"
          ++ "    program and check the resulting configuration works.\n"
          ++ "  "
          ++ pname
          ++ " v2-configure\n"
          ++ "    Reset the local configuration to empty. To check that the\n"
          ++ "    project configuration works, use 'cabal build'.\n"
    , commandDefaultFlags = defaultNixStyleFlags ()
    , commandOptions =
        removeIgnoreProjectOption
          . nixStyleOptions (const [])
    }

-- | To a first approximation, the @configure@ just runs the first phase of
-- the @build@ command where we bring the install plan up to date (thus
-- checking that it's possible).
--
-- The only difference is that @configure@ also allows the user to specify
-- some extra config flags which we save in the file @cabal.project.local@.
--
-- For more details on how this works, see the module
-- "Distribution.Client.ProjectOrchestration"
configureAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
configureAction flags@NixStyleFlags{..} extraArgs globalFlags = do
  (baseCtx, projConfig) <- configureAction' flags extraArgs globalFlags

  if shouldNotWriteFile baseCtx
    then notice v "Config file not written due to flag(s)."
    else writeProjectLocalExtraConfig (distDirLayout baseCtx) projConfig
  where
    v = fromFlagOrDefault normal (configVerbosity configFlags)

configureAction' :: NixStyleFlags () -> [String] -> GlobalFlags -> IO (ProjectBaseContext, ProjectConfig)
configureAction' flags@NixStyleFlags{..} _extraArgs globalFlags = do
  -- TODO: deal with _extraArgs, since flags with wrong syntax end up there

  baseCtx <- establishProjectBaseContext v cliConfig OtherCommand

  let localFile = distProjectFile (distDirLayout baseCtx) "local"
  -- If cabal.project.local already exists, and the flags allow, back up to cabal.project.local~
  let backups = fromFlagOrDefault True $ configBackup configExFlags
      appends = fromFlagOrDefault False $ configAppend configExFlags
      backupFile = localFile <> "~"

  if shouldNotWriteFile baseCtx
    then return (baseCtx, cliConfig)
    else do
      exists <- doesFileExist localFile
      when (exists && backups) $ do
        notice v $
          quote (takeFileName localFile)
            <> " already exists, backing it up to "
            <> quote (takeFileName backupFile)
            <> "."
        copyFile localFile backupFile

      -- If the flag @configAppend@ is set to true, append and do not overwrite
      if exists && appends
        then do
          httpTransport <-
            configureTransport
              v
              (fromNubList . projectConfigProgPathExtra $ projectConfigShared cliConfig)
              (flagToMaybe . projectConfigHttpTransport $ projectConfigBuildOnly cliConfig)
          (CondNode conf imps bs) <-
            runRebuild (distProjectRootDirectory . distDirLayout $ baseCtx) $
              readProjectLocalExtraConfig v httpTransport (distDirLayout baseCtx)
          when (not (null imps && null bs)) $ dieWithException v UnableToPerformInplaceUpdate
          return (baseCtx, conf <> cliConfig)
        else return (baseCtx, cliConfig)
  where
    v = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here
    quote s = "'" <> s <> "'"

-- Config file should not be written when certain flags are present
shouldNotWriteFile :: ProjectBaseContext -> Bool
shouldNotWriteFile baseCtx =
  buildSettingDryRun (buildSettings baseCtx)
    || buildSettingOnlyDownload (buildSettings baseCtx)

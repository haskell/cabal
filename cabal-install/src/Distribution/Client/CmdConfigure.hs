{-# LANGUAGE RecordWildCards #-}
-- | cabal-install CLI command: configure
--
module Distribution.Client.CmdConfigure (
    configureCommand,
    configureAction,
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory
import System.FilePath
import qualified Data.Map as Map

import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectConfig
         ( writeProjectLocalExtraConfig )

import Distribution.Client.NixStyleOptions
         ( NixStyleFlags (..), nixStyleOptions, defaultNixStyleFlags )
import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags(..) )
import Distribution.Simple.Flag
         ( fromFlagOrDefault )
import Distribution.Verbosity
         ( normal )

import Distribution.Simple.Command
         ( CommandUI(..), usageAlternatives, optionName )
import Distribution.Simple.Utils
         ( wrapText, notice )

import Distribution.Client.DistDirLayout
         ( DistDirLayout(..) )

configureCommand :: CommandUI (NixStyleFlags ())
configureCommand = CommandUI {
  commandName         = "v2-configure",
  commandSynopsis     = "Add extra project configuration",
  commandUsage        = usageAlternatives "v2-configure" [ "[FLAGS]" ],
  commandDescription  = Just $ \_ -> wrapText $
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
     ++ "to alter the 'cabal.project' persistently.",
  commandNotes        = Just $ \pname ->
        "Examples:\n"
     ++ "  " ++ pname ++ " v2-configure --with-compiler ghc-7.10.3\n"
     ++ "    Adjust the project configuration to use the given compiler\n"
     ++ "    program and check the resulting configuration works.\n"
     ++ "  " ++ pname ++ " v2-configure\n"
     ++ "    Reset the local configuration to empty and check the overall\n"
     ++ "    project configuration works.\n"

  , commandDefaultFlags = defaultNixStyleFlags ()
  , commandOptions      = filter (\o -> optionName o /= "ignore-project")
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
--
configureAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
configureAction flags@NixStyleFlags {..} _extraArgs globalFlags = do
    --TODO: deal with _extraArgs, since flags with wrong syntax end up there

    baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand

    -- Write out the @cabal.project.local@ so it gets picked up by the
    -- planning phase. If old config exists, then print the contents
    -- before overwriting

    let localFile = distProjectFile (distDirLayout baseCtx) "local"
        -- | Chooses cabal.project.local~, or if it already exists
        -- cabal.project.local~0, cabal.project.local~1 etc.
        firstFreeBackup = firstFreeBackup' (0 :: Int)
        firstFreeBackup' i = do
          let backup = localFile <> "~" <> (if i <= 0 then "" else show (i - 1))
          exists <- doesFileExist backup
          if exists
            then firstFreeBackup' (i + 1)
            else return backup

    -- If cabal.project.local already exists, back up to cabal.project.local~[n]
    exists <- doesFileExist localFile
    when exists $ do
        backup <- firstFreeBackup
        notice verbosity $
          quote (takeFileName localFile) <> " already exists, backing it up to "
          <> quote (takeFileName backup) <> "."
        copyFile localFile backup
    writeProjectLocalExtraConfig (distDirLayout baseCtx)
                                 cliConfig

    buildCtx <-
      runProjectPreBuildPhase verbosity baseCtx $ \elaboratedPlan ->

            -- TODO: Select the same subset of targets as 'CmdBuild' would
            -- pick (ignoring, for example, executables in libraries
            -- we depend on). But we don't want it to fail, so actually we
            -- have to do it slightly differently from build.
            return (elaboratedPlan, Map.empty)

    let baseCtx' = baseCtx {
                      buildSettings = (buildSettings baseCtx) {
                        buildSettingDryRun = True
                      }
                    }

    -- TODO: Hmm, but we don't have any targets. Currently this prints
    -- what we would build if we were to build everything. Could pick
    -- implicit target like "."
    --
    -- TODO: should we say what's in the project (+deps) as a whole?
    printPlan verbosity baseCtx' buildCtx
  where
    verbosity = fromFlagOrDefault normal (configVerbosity configFlags)
    cliConfig = commandLineFlagsToProjectConfig globalFlags flags
                  mempty -- ClientInstallFlags, not needed here
    quote s = "'" <> s <> "'"


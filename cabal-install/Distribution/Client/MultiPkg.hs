{-# LANGUAGE BangPatterns, RecordWildCards, NamedFieldPuns,
             DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

-- | An experimental new UI for cabal for working with multiple packages
-----------------------------------------------------------------------------
module Distribution.Client.MultiPkg (
    -- * Top level CLI commands
    configure,
    build,
    repl,
  ) where

import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectOrchestration
import Distribution.Client.BuildTarget

import Distribution.Client.Setup
         ( GlobalFlags, ConfigFlags, ConfigExFlags, InstallFlags )
import Distribution.Simple.Setup
         ( HaddockFlags )
import Distribution.Verbosity

import Control.Monad (unless)


------------------------------------------------------------------------------
-- * Top level commands: build and configure
------------------------------------------------------------------------------

build :: Verbosity
      -> GlobalFlags
      -> ConfigFlags
      -> ConfigExFlags
      -> InstallFlags
      -> HaddockFlags
      -> [String]
      -> IO ()
build verbosity
      globalFlags
      configFlags configExFlags
      installFlags haddockFlags
      targetStrings = do

    userTargets <- readUserBuildTargets targetStrings

    buildCtx@ProjectBuildContext{buildSettings} <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),
          hookSelectPlanSubset = selectBuildTargets userTargets
        }

    printPlan verbosity buildCtx

    unless (buildSettingDryRun buildSettings) $
      runProjectBuildPhase
        verbosity
        buildCtx
  where
    selectBuildTargets =
      selectTargets
        BuildDefaultComponents
        BuildSpecificComponent


repl :: Verbosity
     -> GlobalFlags
     -> ConfigFlags
     -> ConfigExFlags
     -> InstallFlags
     -> HaddockFlags
     -> [String]
     -> IO ()
repl verbosity
      globalFlags
      configFlags configExFlags
      installFlags haddockFlags
      targetStrings = do

    userTargets <- readUserBuildTargets targetStrings

    buildCtx@ProjectBuildContext{buildSettings} <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning      = \_ _ _ -> return (),
          hookSelectPlanSubset = selectBuildTargets userTargets
        }

    printPlan verbosity buildCtx

    unless (buildSettingDryRun buildSettings) $
      runProjectBuildPhase
        verbosity
        buildCtx
  where
    selectBuildTargets =
      selectTargets
        ReplDefaultComponent
        ReplSpecificComponent


-- To a first approximation, configure just runs the first phase of 'build'
-- where we bring the install plan up to date (thus checking that it's
-- possible).
--
-- The only difference is that 'configure' also allows the user to specify
-- some extra local temporary config flags.
--

configure :: Verbosity
          -> GlobalFlags
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> HaddockFlags
          -> [String]
          -> IO ()
configure verbosity
          globalFlags
          configFlags configExFlags
          installFlags haddockFlags
          _extraArgs = do
    --TODO: deal with _extraArgs, since flags with wrong syntax end up there

    buildCtx <-
      runProjectPreBuildPhase
        verbosity
        ( globalFlags, configFlags, configExFlags
        , installFlags, haddockFlags )
        PreBuildHooks {
          hookPrePlanning = \projectRootDir _ cliConfig ->
            writeProjectLocalExtraConfig projectRootDir cliConfig,

          hookSelectPlanSubset = return
        }

    printPlan
      verbosity
      buildCtx {
        buildSettings = (buildSettings buildCtx) {
          buildSettingDryRun = True
        }
      }

    --TODO: Hmm, but we don't have any targets. Currently this prints what we
    -- would build if we were to build everything. Could pick implicit target like "."
    --TODO: should we say what's in the project (+deps) as a whole?


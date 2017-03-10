-------------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'new-exec' command for running an arbitrary executable
-- in an environment suited to the part of the store built for a project.
-------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.CmdExec
  ( execAction
  , execCommand
  ) where

import Distribution.Client.DistDirLayout
  ( DistDirLayout(..)
  )
import Distribution.Client.InstallPlan
  ( GenericPlanPackage(..)
  , toGraph
  )
import Distribution.Client.Setup
  ( ConfigExFlags
  , ConfigFlags(configVerbosity)
  , GlobalFlags
  , InstallFlags
  , installCommand
  )
import Distribution.Client.ProjectOrchestration
  ( ProjectBuildContext(..)
  , PreBuildHooks(..)
  , runProjectPreBuildPhase
  )
import Distribution.Client.ProjectPlanOutput
  ( updatePostBuildProjectStatus
  , createPackageEnvironment
  )
import qualified Distribution.Client.ProjectPlanning as Planning
import Distribution.Client.ProjectPlanning
  ( ElaboratedInstallPlan
  , ElaboratedSharedConfig(..)
  )
import Distribution.Simple.Command
  ( CommandUI(..)
  )
import Distribution.Simple.Program.Db
  ( modifyProgramSearchPath
  , requireProgram
  )
import Distribution.Simple.Program.Find
  ( ProgramSearchPathEntry(..)
  )
import Distribution.Simple.Program.Run
  ( programInvocation
  , runProgramInvocation
  )
import Distribution.Simple.Program.Types
  ( programOverrideEnv
  , simpleProgram
  )
import Distribution.Simple.Setup
  ( HaddockFlags
  , fromFlagOrDefault
  )
import Distribution.Simple.Utils
  ( die
  , info
  , withTempDirectory
  , wrapText
  )
import Distribution.Verbosity
  ( Verbosity
  , normal
  )

import Prelude ()
import Distribution.Client.Compat.Prelude

import Data.Set (Set)
import qualified Data.Set as S

execCommand :: CommandUI (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
execCommand = installCommand
  { commandName = "new-exec"
  , commandSynopsis = "Give a command access to the store."
  , commandUsage = \pname ->
    "Usage: " ++ pname ++ " new-exec [FLAGS] [--] COMMAND [--] [ARGS]\n"
  , commandDescription = Just $ \pname -> wrapText $
       "During development it is often useful to run build tasks and perform"
    ++ " one-off program executions to experiment with the behavior of build"
    ++ " tools. It is convenient to run these tools in the same way " ++ pname
    ++ " itself would. The `" ++ pname ++ " new-exec` command provides a way to"
    ++ " do so.\n"
    ++ "\n"
    ++ "Compiler tools will be configured to see the same subset of the store"
    ++ " that builds would see. The PATH is modified to make all executables in"
    ++ " the dependency tree available (provided they have been built already)."
    ++ " Commands are also rewritten in the way cabal itself would. For"
    ++ " example, `" ++ pname ++ " new-exec ghc` will consult the configuration"
    ++ " to choose an appropriate version of ghc and to include any"
    ++ " ghc-specific flags requested."
  , commandNotes = Nothing
  }

execAction :: (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags)
           -> [String] -> GlobalFlags -> IO ()
execAction (configFlags, configExFlags, installFlags, haddockFlags)
           extraArgs globalFlags = do
  let verbosity = fromFlagOrDefault normal (configVerbosity configFlags)

  -- To set up the environment, we'd like to select the libraries in our
  -- dependency tree that we've already built. So first we set up an install
  -- plan, but we walk the dependency tree without first executing the plan.
  buildCtx <- runProjectPreBuildPhase
    verbosity
    (globalFlags, configFlags, configExFlags, installFlags, haddockFlags)
    PreBuildHooks
      { hookPrePlanning = \_ _ _ -> return ()
      , hookSelectPlanSubset = \_ -> return
      }

  -- We use the build status below to decide what libraries to include in the
  -- compiler environment, but we don't want to actually build anything. So we
  -- pass mempty to indicate that nothing happened and we just want the current
  -- status.
  buildStatus <- updatePostBuildProjectStatus
    verbosity
    (distDirLayout buildCtx)
    (elaboratedPlanToExecute buildCtx)
    (pkgsBuildStatus buildCtx)
    mempty

  -- Now that we have the packages, set up the environment. We accomplish this
  -- by creating an environment file that selects the databases and packages we
  -- computed in the previous step, and setting an environment variable to
  -- point at the file.
  withTempDirectory
    verbosity
    (distTempDirectory (distDirLayout buildCtx))
    "environment."
    $ \tmpDir -> do
      envOverrides <- createPackageEnvironment
        verbosity
        tmpDir
        (elaboratedPlanToExecute buildCtx)
        (elaboratedShared buildCtx)
        buildStatus

      -- Some dependencies may have executables. Let's put those on the PATH.
      extraPaths <- pathAdditions verbosity buildCtx
      let programDb = modifyProgramSearchPath
                      (map ProgramSearchPathDir extraPaths ++)
                    . pkgConfigCompilerProgs
                    . elaboratedShared
                    $ buildCtx

      case extraArgs of
        exe:args -> do
          (program, _) <- requireProgram verbosity (simpleProgram exe) programDb
          let program'   = withOverrides envOverrides program
              invocation = programInvocation program' args
          runProgramInvocation verbosity invocation
        [] -> die "Please specify an executable to run"
  where
  withOverrides env program = program
    { programOverrideEnv = programOverrideEnv program ++ env }

pathAdditions :: Verbosity -> ProjectBuildContext -> IO [FilePath]
pathAdditions verbosity ProjectBuildContext{..} = do
  info verbosity . unlines $ "Including the following directories in PATH:"
                           : paths
  return paths
  where
  paths = S.toList
        $ binDirectories distDirLayout elaboratedShared elaboratedPlanToExecute

binDirectories
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> Set FilePath
binDirectories layout config = fromElaboratedInstallPlan where
  fromElaboratedInstallPlan = fromGraph . toGraph
  fromGraph = foldMap fromPlan
  fromSrcPkg = S.fromList . Planning.binDirectories layout config

  fromPlan (PreExisting _) = mempty
  fromPlan (Configured pkg) = fromSrcPkg pkg
  fromPlan (Installed pkg) = fromSrcPkg pkg

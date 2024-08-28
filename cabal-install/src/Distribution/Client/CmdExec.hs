-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'v2-exec' command for running an arbitrary executable
-- in an environment suited to the part of the store built for a project.
module Distribution.Client.CmdExec
  ( execAction
  , execCommand
  ) where

import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  )
import Distribution.Client.InstallPlan
  ( GenericPlanPackage (..)
  , toGraph
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig.Types
  ( ProjectConfig (projectConfigShared)
  , ProjectConfigShared (projectConfigProgPathExtra)
  )
import Distribution.Client.ProjectFlags
  ( removeIgnoreProjectOption
  )
import Distribution.Client.ProjectOrchestration
  ( BuildTimeSettings (..)
  , CurrentCommand (..)
  , ProjectBaseContext (..)
  , ProjectBuildContext (..)
  , commandLineFlagsToProjectConfig
  , distDirLayout
  , establishProjectBaseContext
  , runProjectPreBuildPhase
  )
import Distribution.Client.ProjectPlanOutput
  ( PostBuildProjectStatus
  , argsEquivalentOfGhcEnvironmentFile
  , createPackageEnvironment
  , updatePostBuildProjectStatus
  )
import Distribution.Client.ProjectPlanning
  ( ElaboratedInstallPlan
  , ElaboratedSharedConfig (..)
  )
import qualified Distribution.Client.ProjectPlanning as Planning
import Distribution.Client.ProjectPlanning.Types
  ( dataDirsEnvironmentForPlan
  )
import Distribution.Client.Setup
  ( ConfigFlags (configCommonFlags)
  , GlobalFlags
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  )
import Distribution.Simple.Flag
  ( fromFlagOrDefault
  )
import Distribution.Simple.GHC
  ( GhcImplInfo (supportsPkgEnvFiles)
  , getImplInfo
  )
import Distribution.Simple.Program
  ( ConfiguredProgram
  , programDefaultArgs
  , programOverrideEnv
  , programPath
  , simpleProgram
  )
import Distribution.Simple.Program.Db
  ( configuredPrograms
  , prependProgramSearchPath
  , requireProgram
  )
import Distribution.Simple.Program.Run
  ( programInvocation
  , runProgramInvocation
  )
import Distribution.Simple.Setup (CommonSetupFlags (..))
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , dieWithException
  , notice
  , withTempDirectory
  , wrapText
  )
import Distribution.Utils.NubList
  ( fromNubList
  )
import Distribution.Verbosity
  ( normal
  )

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Data.Map as M
import qualified Data.Set as S
import Distribution.Client.Errors

execCommand :: CommandUI (NixStyleFlags ())
execCommand =
  CommandUI
    { commandName = "v2-exec"
    , commandSynopsis = "Give a command access to the store."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " v2-exec [FLAGS] [--] COMMAND [--] [ARGS]\n"
    , commandDescription = Just $ \pname ->
        wrapText $
          "During development it is often useful to run build tasks and perform"
            ++ " one-off program executions to experiment with the behavior of build"
            ++ " tools. It is convenient to run these tools in the same way "
            ++ pname
            ++ " itself would. The `"
            ++ pname
            ++ " v2-exec` command provides a way to"
            ++ " do so.\n"
            ++ "\n"
            ++ "Compiler tools will be configured to see the same subset of the store"
            ++ " that builds would see. The PATH is modified to make all executables in"
            ++ " the dependency tree available (provided they have been built already)."
            ++ " Commands are also rewritten in the way cabal itself would. For"
            ++ " example, `"
            ++ pname
            ++ " v2-exec ghc` will consult the configuration"
            ++ " to choose an appropriate version of ghc and to include any"
            ++ " ghc-specific flags requested."
    , commandNotes = Nothing
    , commandOptions =
        removeIgnoreProjectOption
          . nixStyleOptions (const [])
    , commandDefaultFlags = defaultNixStyleFlags ()
    }

execAction :: NixStyleFlags () -> [String] -> GlobalFlags -> IO ()
execAction flags@NixStyleFlags{..} extraArgs globalFlags = do
  baseCtx <- establishProjectBaseContext verbosity cliConfig OtherCommand

  -- To set up the environment, we'd like to select the libraries in our
  -- dependency tree that we've already built. So first we set up an install
  -- plan, but we walk the dependency tree without first executing the plan.
  buildCtx <-
    runProjectPreBuildPhase
      verbosity
      baseCtx
      (\plan -> return (plan, M.empty))

  -- We use the build status below to decide what libraries to include in the
  -- compiler environment, but we don't want to actually build anything. So we
  -- pass mempty to indicate that nothing happened and we just want the current
  -- status.
  buildStatus <-
    updatePostBuildProjectStatus
      verbosity
      (distDirLayout baseCtx)
      (elaboratedPlanOriginal buildCtx)
      (pkgsBuildStatus buildCtx)
      mempty

  -- Some dependencies may have executables. Let's put those on the PATH.
  let extraPaths = pathAdditions baseCtx buildCtx
      pkgProgs = pkgConfigCompilerProgs (elaboratedShared buildCtx)
      extraEnvVars =
        dataDirsEnvironmentForPlan
          (distDirLayout baseCtx)
          (elaboratedPlanToExecute buildCtx)

  programDb <-
    prependProgramSearchPath verbosity extraPaths extraEnvVars pkgProgs

  -- Now that we have the packages, set up the environment. We accomplish this
  -- by creating an environment file that selects the databases and packages we
  -- computed in the previous step, and setting an environment variable to
  -- point at the file.
  -- In case ghc is too old to support environment files,
  -- we pass the same info as arguments
  let compiler = pkgConfigCompiler $ elaboratedShared buildCtx
      envFilesSupported = supportsPkgEnvFiles (getImplInfo compiler)
  case extraArgs of
    [] -> dieWithException verbosity SpecifyAnExecutable
    exe : args -> do
      (program, _) <- requireProgram verbosity (simpleProgram exe) programDb
      let argOverrides =
            argsEquivalentOfGhcEnvironmentFile
              compiler
              (distDirLayout baseCtx)
              (elaboratedPlanOriginal buildCtx)
              buildStatus
          programIsConfiguredCompiler =
            matchCompilerPath
              (elaboratedShared buildCtx)
              program
          argOverrides' =
            if envFilesSupported
              || not programIsConfiguredCompiler
              then []
              else argOverrides

      ( if envFilesSupported
          then withTempEnvFile verbosity baseCtx buildCtx buildStatus
          else \f -> f []
        )
        $ \envOverrides -> do
          let program' =
                withOverrides
                  envOverrides
                  argOverrides'
                  program
              invocation = programInvocation program' args
              dryRun =
                buildSettingDryRun (buildSettings baseCtx)
                  || buildSettingOnlyDownload (buildSettings baseCtx)

          if dryRun
            then notice verbosity "Running of executable suppressed by flag(s)"
            else runProgramInvocation verbosity invocation
  where
    verbosity = fromFlagOrDefault normal (setupVerbosity $ configCommonFlags configFlags)
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here
    withOverrides env args program =
      program
        { programOverrideEnv = programOverrideEnv program ++ env
        , programDefaultArgs = programDefaultArgs program ++ args
        }

matchCompilerPath :: ElaboratedSharedConfig -> ConfiguredProgram -> Bool
matchCompilerPath elaboratedShared program =
  programPath program
    `elem` (programPath <$> configuredCompilers)
  where
    configuredCompilers = configuredPrograms $ pkgConfigCompilerProgs elaboratedShared

-- | Execute an action with a temporary .ghc.environment file reflecting the
-- current environment. The action takes an environment containing the env
-- variable which points ghc to the file.
withTempEnvFile
  :: Verbosity
  -> ProjectBaseContext
  -> ProjectBuildContext
  -> PostBuildProjectStatus
  -> ([(String, Maybe String)] -> IO a)
  -> IO a
withTempEnvFile verbosity baseCtx buildCtx buildStatus action = do
  let tmpDirTemplate = distTempDirectory (distDirLayout baseCtx)
  createDirectoryIfMissingVerbose verbosity True tmpDirTemplate
  withTempDirectory
    verbosity
    tmpDirTemplate
    "environment."
    ( \tmpDir -> do
        envOverrides <-
          createPackageEnvironment
            verbosity
            tmpDir
            (elaboratedPlanToExecute buildCtx)
            (elaboratedShared buildCtx)
            buildStatus
        action envOverrides
    )

-- | Get paths to all dependency executables to be included in PATH.
pathAdditions :: ProjectBaseContext -> ProjectBuildContext -> [FilePath]
pathAdditions ProjectBaseContext{..} ProjectBuildContext{..} =
  paths ++ cabalConfigPaths
  where
    cabalConfigPaths =
      fromNubList
        . projectConfigProgPathExtra
        . projectConfigShared
        $ projectConfig
    paths =
      S.toList $
        binDirectories distDirLayout elaboratedShared elaboratedPlanToExecute

-- | Get paths to all dependency executables to be included in PATH.
binDirectories
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> Set FilePath
binDirectories layout config = fromElaboratedInstallPlan
  where
    fromElaboratedInstallPlan = fromGraph . toGraph
    fromGraph = foldMap fromPlan
    fromSrcPkg = S.fromList . Planning.binDirectories layout config

    fromPlan (PreExisting _) = mempty
    fromPlan (Configured pkg) = fromSrcPkg pkg
    fromPlan (Installed pkg) = fromSrcPkg pkg

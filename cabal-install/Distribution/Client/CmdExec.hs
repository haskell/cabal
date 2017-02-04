-------------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'new-exec' command for running an arbitrary executable
-- in an environment suited to the part of the store built for a project.
-------------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving #-}
module Distribution.Client.CmdExec
  ( execAction
  , execCommand
  ) where

import Distribution.Client.DistDirLayout
  ( distTempDirectory
  )
import Distribution.Client.Setup
  ( GlobalFlags(..)
  , ExecFlags(..)
  , defaultExecFlags
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
import Distribution.Simple.Command
  ( CommandUI(..)
  )
import Distribution.Simple.Program.Run
  ( ProgramInvocation(..)
  , runProgramInvocation
  , simpleProgramInvocation
  )
import Distribution.Simple.Setup
  ( fromFlag
  ,  optionDistPref,  optionVerbosity
  ,  configDistPref,  configVerbosity
  , haddockDistPref, haddockVerbosity
  )
import Distribution.Simple.Utils
  ( die
  , withTempDirectory
  , wrapText
  )

execCommand :: CommandUI ExecFlags
execCommand = CommandUI
  { commandName = "new-exec"
  , commandSynopsis = "Give a command access to the store."
  , commandUsage = \pname ->
    "Usage: " ++ pname ++ " new-exec [FLAGS] [--] COMMAND [--] [ARGS]\n"
  , commandDescription = Just $ \_pname -> wrapText $
    "TODO"
  , commandNotes = Nothing
  , commandDefaultFlags = defaultExecFlags
  , commandOptions = \showOrParseArgs ->
    [ optionVerbosity execVerbosity (\v flags -> flags { execVerbosity = v })
    , optionDistPref
        execDistPref (\v flags -> flags { execDistPref = v })
        showOrParseArgs
    ]
  }

execAction :: ExecFlags -> [String] -> GlobalFlags -> IO ()
execAction execFlags extraArgs globalFlags = do
  let verbosity = fromFlag (execVerbosity execFlags)

  -- To set up the environment, we'd like to select the libraries in our
  -- dependency tree that we've already built. So first we set up an install
  -- plan, but we walk the dependency tree without first executing the plan.
  --
  -- TODO: We set a lot of default settings here (with mempty). It might be
  -- worth walking through each of the settings we default and making sure they
  -- shouldn't become ExecFlags.
  buildCtx <- runProjectPreBuildPhase
    verbosity
    ( globalFlags
    , mempty
        { configDistPref = execDistPref execFlags
        , configVerbosity = execVerbosity execFlags
        }
    , mempty
    , mempty
    , mempty
        { haddockDistPref = execDistPref execFlags
        , haddockVerbosity = execVerbosity execFlags
        }
    )
    PreBuildHooks
      { hookPrePlanning = \_ _ _ -> return ()
      , hookSelectPlanSubset = \_ -> return
      }
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

      -- TODO: discuss PATH munging with #hackage

      case extraArgs of
        exe:args -> runProgramInvocation
          verbosity
          (simpleProgramInvocation exe args)
            { progInvokeEnv = envOverrides
            }
        [] -> die "Please specify an executable to run"

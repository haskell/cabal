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
import Distribution.Client.ProjectPlanning
  ( ElaboratedInstallPlan
  , ElaboratedSharedConfig(..)
  , binDirectory
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
  ( fromFlag
  ,  optionDistPref,  optionVerbosity
  ,  configDistPref,  configVerbosity
  , haddockDistPref, haddockVerbosity
  )
import Distribution.Simple.Utils
  ( debug
  , die
  , info
  , withTempDirectory
  , wrapText
  )
import Distribution.Verbosity
  ( Verbosity
  )

import Control.Monad (filterM)
import Data.Set (Set)
import qualified Data.Set as S
import System.Directory (executable, getPermissions, listDirectory)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)

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
  debug verbosity $ "Considering the following directories for inclusion in PATH:"
  mapM_ (debug verbosity) paths
  occupiedPaths <- filterM hasExecutable (S.toAscList paths)
  info verbosity $ "Including the following directories in PATH:"
  mapM_ (info verbosity) occupiedPaths
  return occupiedPaths
  where
  paths = binDirectories distDirLayout elaboratedShared elaboratedPlanToExecute

binDirectories
  :: DistDirLayout
  -> ElaboratedSharedConfig
  -> ElaboratedInstallPlan
  -> Set FilePath
binDirectories layout config = fromElaboratedInstallPlan where
  fromElaboratedInstallPlan = fromGraph . toGraph
  fromGraph = foldMap fromPlan
  fromSrcPkg pkg = S.singleton (binDirectory layout config pkg)

  fromPlan (PreExisting _) = mempty
  fromPlan (Configured pkg) = fromSrcPkg pkg
  fromPlan (Installed pkg) = fromSrcPkg pkg

-- | Check whether a directory contains an executable.
hasExecutable :: FilePath -> IO Bool
hasExecutable dir = catchIOError
  (listDirectory dir >>= anyM (isExecutable . (dir</>)))
  (\_ -> return False)

-- | Check whether a file is an executable.
isExecutable :: FilePath -> IO Bool
isExecutable file = catchIOError
  (executable <$> getPermissions file)
  (\_ -> return False)

-- | Like 'any', but short-circuits side-effects, too.
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM f (x:xs) = do
  fx <- f x
  if fx then return True else anyM f xs

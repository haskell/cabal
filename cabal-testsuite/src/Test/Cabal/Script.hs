{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Functionality for invoking Haskell scripts with the correct
-- package database setup.
module Test.Cabal.Script (
    ScriptEnv(..),
    mkScriptEnv,
    runnerGhcArgs,
    runnerCommand,
    runghc,
) where

import Test.Cabal.Run
import Test.Cabal.ScriptEnv0

import Distribution.Backpack
import Distribution.Types.ModuleRenaming
import Distribution.Utils.NubList
import Distribution.Utils.Path
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program
import Distribution.Simple.Compiler
import Distribution.Verbosity
import Distribution.System
import Distribution.Simple.Setup (Flag(..))

import qualified Data.Monoid as M


-- | The runner environment, which contains all of the important
-- parameters for invoking GHC.  Mostly subset of 'LocalBuildInfo'.
data ScriptEnv = ScriptEnv
        { runnerProgramDb       :: ProgramDb
        , runnerPackageDbStack  :: PackageDBStack
        , runnerVerbosity       :: Verbosity
        , runnerPlatform        :: Platform
        , runnerCompiler        :: Compiler
        , runnerPackages        :: [(OpenUnitId, ModuleRenaming)]
        , runnerWithSharedLib   :: Bool
        }
    deriving Show

{-

-- | Convert package database into absolute path, so that
-- if we change working directories in a subprocess we get the correct database.
canonicalizePackageDB :: PackageDB -> IO PackageDB
canonicalizePackageDB (SpecificPackageDB path)
    = SpecificPackageDB `fmap` canonicalizePath path
canonicalizePackageDB x = return x

-}

-- | Create a 'ScriptEnv' from a 'LocalBuildInfo' configured with
-- the GHC that we want to use.
mkScriptEnv :: Verbosity -> IO ScriptEnv
mkScriptEnv verbosity =
  return $ ScriptEnv
    { runnerVerbosity       = verbosity
    , runnerProgramDb       = lbiProgramDb
    , runnerPackageDbStack  = lbiPackageDbStack
    , runnerPlatform        = lbiPlatform
    , runnerCompiler        = lbiCompiler
    -- NB: the set of packages available to test.hs scripts will COINCIDE
    -- with the dependencies on the cabal-testsuite library
    , runnerPackages        = lbiPackages
    , runnerWithSharedLib   = lbiWithSharedLib
    }

-- | Run a script with 'runghc', under the 'ScriptEnv'.
runghc :: ScriptEnv -> Maybe FilePath -> [(String, Maybe String)]
       -> FilePath -> [String] -> IO Result
runghc senv mb_cwd env_overrides script_path args = do
    (real_path, real_args) <- runnerCommand senv mb_cwd env_overrides script_path args
    run (runnerVerbosity senv) mb_cwd env_overrides real_path real_args Nothing

-- | Compute the command line which should be used to run a Haskell
-- script with 'runghc'.
runnerCommand :: ScriptEnv -> Maybe FilePath -> [(String, Maybe String)]
              -> FilePath -> [String] -> IO (FilePath, [String])
runnerCommand senv mb_cwd _env_overrides script_path args = do
    (prog, _) <- requireProgram verbosity runghcProgram (runnerProgramDb senv)
    return $
      (programPath prog,
        runghc_args ++ ["--"] ++ map ("--ghc-arg="++) ghc_args ++ [script_path] ++ args)
  where
    verbosity = runnerVerbosity senv
    runghc_args = []
    ghc_args = runnerGhcArgs senv mb_cwd

-- | Compute the GHC flags to invoke 'runghc' with under a 'ScriptEnv'.
runnerGhcArgs :: ScriptEnv -> Maybe FilePath -> [String]
runnerGhcArgs senv mb_cwd =
  renderGhcOptions (runnerCompiler senv) (runnerPlatform senv) ghc_options
  where
    ghc_options = M.mempty { ghcOptPackageDBs = runnerPackageDbStack senv
                           , ghcOptPackages   = toNubListR (runnerPackages senv)
                           , ghcOptHideAllPackages = Flag True
                           -- Avoid picking stray module files that look
                           -- like our imports...
                           , ghcOptSourcePathClear = Flag True
                           -- ... yet retain the current directory as an included
                           -- directory, e.g. so that we can compile a Setup.hs
                           -- script which imports a locally defined module.
                           -- See the PackageTests/SetupDep test.
                           , ghcOptSourcePath = toNubListR $
                              case mb_cwd of
                                Nothing -> []
                                Just {} -> [sameDirectory]
                            }

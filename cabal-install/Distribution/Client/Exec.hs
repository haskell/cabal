-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Exec
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Implementation of the 'exec' command. Runs an arbitrary executable in an
-- environment suitable for making use of the sandbox.
-----------------------------------------------------------------------------

module Distribution.Client.Exec ( exec
                                ) where

import Distribution.Client.Sandbox.PackageEnvironment (sandboxPackageDBPath)
import Distribution.Client.Sandbox.Types              (UseSandbox (..))

import Distribution.Simple.Compiler    (Compiler)
import Distribution.Simple.GHC         (ghcGlobalPackageDB)
import Distribution.Simple.Program     (ghcProgram, lookupProgram)
import Distribution.Simple.Program.Db  (ProgramDb, requireProgram, modifyProgramSearchPath)
import Distribution.Simple.Program.Find (ProgramSearchPathEntry(..))
import Distribution.Simple.Program.Run (programInvocation, runProgramInvocation)
import Distribution.Simple.Program.Types ( simpleProgram, ConfiguredProgram(..) )
import Distribution.Simple.Utils       (die)

import Distribution.System    (Platform)
import Distribution.Verbosity (Verbosity)

import System.FilePath (searchPathSeparator, (</>))
import Control.Applicative ((<$>))
import Data.Traversable as T


-- | Execute the given command in the package's environment.
--
-- The given command is executed with GHC configured to use the correct
-- package database and with the sandbox bin directory added to the PATH.
exec :: Verbosity
     -> UseSandbox
     -> Compiler
     -> Platform
     -> ProgramDb
     -> [String]
     -> IO ()
exec verbosity useSandbox comp platform programDb extraArgs =
    case extraArgs of
        (exe:args) -> do
            program <- requireProgram' verbosity useSandbox programDb exe
            env <- ((++) (programOverrideEnv program)) <$> environmentOverrides
            let invocation = programInvocation
                                 program { programOverrideEnv = env }
                                 args
            runProgramInvocation verbosity invocation

        [] -> die "Please specify an executable to run"
  where
    environmentOverrides = 
        case useSandbox of
            NoSandbox -> return []
            (UseSandbox sandboxDir) ->
                sandboxEnvironment verbosity sandboxDir comp platform programDb


-- | Return the package's sandbox environment.
--
-- The environment sets GHC_PACKAGE_PATH so that GHC will use the sandbox.
sandboxEnvironment :: Verbosity
                   -> FilePath
                   -> Compiler
                   -> Platform
                   -> ProgramDb
                   -> IO [(String, Maybe String)]
sandboxEnvironment verbosity sandboxDir comp platform programDb = do
    mGlobalPackageDb <- T.sequence $ ghcGlobalPackageDB verbosity
                                  <$> lookupProgram ghcProgram programDb
    case mGlobalPackageDb of
        Nothing  -> die "exec only works with GHC"
        Just gDb -> return $ overrides gDb
  where
    overrides gDb = [ ("GHC_PACKAGE_PATH", ghcPackagePath gDb) ]

    ghcPackagePath gDb =
        let s = sandboxPackageDBPath sandboxDir comp platform
            in Just $ prependToSearchPath gDb s

    prependToSearchPath path newValue =
        newValue ++ [searchPathSeparator] ++ path


-- | Check that a program is configured and available to be run. If
-- a sandbox is available check in the sandbox's directory.
requireProgram' :: Verbosity
                -> UseSandbox
                -> ProgramDb
                -> String
                -> IO ConfiguredProgram
requireProgram' verbosity useSandbox programDb exe = do
    (program, _) <- requireProgram
                        verbosity
                        (simpleProgram exe)
                        updateSearchPath
    return program
  where
    updateSearchPath =
        flip modifyProgramSearchPath programDb $ \searchPath ->
            case useSandbox of
                NoSandbox -> searchPath
                UseSandbox sandboxDir ->
                    ProgramSearchPathDir (sandboxDir </> "bin") : searchPath

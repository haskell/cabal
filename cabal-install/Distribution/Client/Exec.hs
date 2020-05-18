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

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Simple.Compiler    (Compiler)
import Distribution.Simple.Program.Db  (ProgramDb, requireProgram, modifyProgramSearchPath)
import Distribution.Simple.Program.Run (programInvocation, runProgramInvocation)
import Distribution.Simple.Program.Types ( simpleProgram, ConfiguredProgram(..) )
import Distribution.Simple.Utils       (die')

import Distribution.System    (Platform(..))

-- | Execute the given command in the package's environment.
--
-- The given command is executed with GHC configured to use the correct
-- package database and with the sandbox bin directory added to the PATH.
exec :: Verbosity
     -> Compiler
     -> Platform
     -> ProgramDb
     -> [String]
     -> IO ()
exec verbosity _comp _platform programDb extraArgs =
    case extraArgs of
        (exe:args) -> do
            program <- requireProgram' verbosity programDb exe
            env <- environmentOverrides (programOverrideEnv program)
            let invocation = programInvocation
                                 program { programOverrideEnv = env }
                                 args
            runProgramInvocation verbosity invocation

        [] -> die' verbosity "Please specify an executable to run"
  where
    environmentOverrides env = return env

-- | Check that a program is configured and available to be run. If
-- a sandbox is available check in the sandbox's directory.
requireProgram' :: Verbosity
                -> ProgramDb
                -> String
                -> IO ConfiguredProgram
requireProgram' verbosity programDb exe = do
    (program, _) <- requireProgram
                        verbosity
                        (simpleProgram exe)
                        updateSearchPath
    return program
  where
    updateSearchPath =
        flip modifyProgramSearchPath programDb $ \searchPath -> searchPath

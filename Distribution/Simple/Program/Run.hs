-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Run
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides a data type for program invocations and functions to
-- run them.

module Distribution.Simple.Program.Run (
    ProgramInvocation(..),
    emptyProgramInvocation,
    simpleProgramInvocation,
    programInvocation,

    runProgramInvocation,
    getProgramInvocationOutput,

  ) where

import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..), programPath )
import Distribution.Simple.Utils
         ( die, rawSystemExit, rawSystemStdin, rawSystemStdout )
import Distribution.Verbosity
         ( Verbosity )


-- | Represents a specific invocation of a specific program.
--
-- This is used as an intermediate type between deciding how to call a program
-- and actually doing it. This provides the opportunity to the caller to
-- adjust how the program will be called. These invocations can either be run
-- directly or turned into shell or batch scripts.
--
data ProgramInvocation = ProgramInvocation {
       progInvokePath  :: FilePath,
       progInvokeArgs  :: [String],
       progInvokeEnv   :: [(String, String)],
       progInvokeCwd   :: Maybe FilePath,
       progInvokeInput :: Maybe String
     }

emptyProgramInvocation :: ProgramInvocation
emptyProgramInvocation =
  ProgramInvocation {
    progInvokePath  = "",
    progInvokeArgs  = [],
    progInvokeEnv   = [],
    progInvokeCwd   = Nothing,
    progInvokeInput = Nothing
  }

simpleProgramInvocation :: FilePath -> [String] -> ProgramInvocation
simpleProgramInvocation path args =
  emptyProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args
  }

programInvocation :: ConfiguredProgram -> [String] -> ProgramInvocation
programInvocation prog extraArgs =
  emptyProgramInvocation {
    progInvokePath = programPath prog,
    progInvokeArgs = programArgs prog ++ extraArgs
  }


runProgramInvocation :: Verbosity -> ProgramInvocation -> IO ()
runProgramInvocation verbosity
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = [],
    progInvokeCwd   = Nothing,
    progInvokeInput = Nothing
  } =
  rawSystemExit verbosity path args

runProgramInvocation verbosity
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = [],
    progInvokeCwd   = Nothing,
    progInvokeInput = Just input
  } =
  rawSystemStdin verbosity path args input

runProgramInvocation _ _ =
   die "runProgramInvocation: not yet implemented for this form of invocation"


getProgramInvocationOutput :: Verbosity -> ProgramInvocation -> IO String
getProgramInvocationOutput verbosity
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = envExtra,
    progInvokeCwd   = mcwd,
    progInvokeInput = Nothing
  } =
  rawSystemStdout verbosity path args

getProgramInvocationOutput _ _ =
   die "getProgramInvocationOutput: not yet implemented for this form of invocation"

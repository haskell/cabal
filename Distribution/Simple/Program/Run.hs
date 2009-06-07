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
    multiStageProgramInvocation,

    runProgramInvocation,
    getProgramInvocationOutput,

  ) where

import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..), programPath )
import Distribution.Simple.Utils
         ( die, rawSystemExit, rawSystemStdin, rawSystemStdout )
import Distribution.Verbosity
         ( Verbosity )

import Data.List
         ( foldl', unfoldr )

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
    progInvokeEnv   = [],
    progInvokeCwd   = Nothing,
    progInvokeInput = Nothing
  } =
  rawSystemStdout verbosity path args

getProgramInvocationOutput _ _ =
   die "getProgramInvocationOutput: not yet implemented for this form of invocation"


-- | Like the unix xargs program. Useful for when we've got very long command
-- lines that might overflow an OS limit on command line length and so you
-- need to invoke a command multiple times to get all the args in.
--
-- It takes four template invocations corresponding to the simple, initial,
-- middle and last invocations. If the number of args given is small enough
-- that we can get away with just a single invocation then the simple one is
-- used:
--
-- > $ simple args
--
-- If the number of args given means that we need to use multiple invocations
-- then the templates for the initial, middle and last invocations are used:
--
-- > $ initial args_0
-- > $ middle  args_1
-- > $ middle  args_2
-- >   ...
-- > $ final   args_n
--
multiStageProgramInvocation
  :: ProgramInvocation
  -> (ProgramInvocation, ProgramInvocation, ProgramInvocation)
  -> [String]
  -> [ProgramInvocation]
multiStageProgramInvocation simple (initial, middle, final) args =

  let argSize inv  = length (progInvokePath inv)
                   + foldl' (\s a -> length a + 1 + s) 1 (progInvokeArgs inv)
      fixedArgSize = maximum (map argSize [simple, initial, middle, final])
      chunkSize    = maxCommandLineSize - fixedArgSize

   in case splitChunks chunkSize args of
        []     -> [ simple ]

        [c]    -> [ simple  `appendArgs` c ]

        [c,c'] -> [ initial `appendArgs` c ]
               ++ [ final   `appendArgs` c']

        (c:cs) -> [ initial `appendArgs` c ]
               ++ [ middle  `appendArgs` c'| c' <- init cs ]
               ++ [ final   `appendArgs` c'| let c' = last cs ]

  where
    inv `appendArgs` as = inv { progInvokeArgs = progInvokeArgs inv ++ as }

    splitChunks len = unfoldr $ \s ->
      if null s then Nothing
                else Just (chunk len s)

    chunk len (s:_) | length s >= len = error toolong
    chunk len ss    = chunk' [] len ss

    chunk' acc _   []     = (reverse acc,[])
    chunk' acc len (s:ss)
      | len' < len = chunk' (s:acc) (len-len'-1) ss
      | otherwise  = (reverse acc, s:ss)
      where len' = length s

    toolong = "multiStageProgramInvocation: a single program arg is larger "
           ++ "than the maximum command line length!"


--FIXME: discover this at configure time or runtime on unix
-- The value is 32k on Windows and posix specifies a minimum of 4k
-- but all sensible unixes use more than 4k.
-- we could use getSysVar ArgumentLimit but that's in the unix lib
--
maxCommandLineSize :: Int
maxCommandLineSize = 30 * 1024

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
    IOEncoding(..),
    emptyProgramInvocation,
    simpleProgramInvocation,
    programInvocation,
    multiStageProgramInvocation,

    runProgramInvocation,
    getProgramInvocationOutput,

    getEffectiveEnvironment,
  ) where

import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..), programPath )
import Distribution.Simple.Utils
         ( die, rawSystemExit, rawSystemIOWithEnv, rawSystemStdInOut
         , toUTF8, fromUTF8, normaliseLineEndings )
import Distribution.Verbosity
         ( Verbosity )

import Data.List
         ( foldl', unfoldr )
import qualified Data.Map as Map
import Control.Monad
         ( when )
import System.Exit
         ( ExitCode(..), exitWith )
import Distribution.Compat.Environment
         ( getEnvironment )

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
       progInvokeEnv   :: [(String, Maybe String)],
       progInvokeCwd   :: Maybe FilePath,
       progInvokeInput :: Maybe String,
       progInvokeInputEncoding  :: IOEncoding,
       progInvokeOutputEncoding :: IOEncoding
     }

data IOEncoding = IOEncodingText   -- locale mode text
                | IOEncodingUTF8   -- always utf8

emptyProgramInvocation :: ProgramInvocation
emptyProgramInvocation =
  ProgramInvocation {
    progInvokePath  = "",
    progInvokeArgs  = [],
    progInvokeEnv   = [],
    progInvokeCwd   = Nothing,
    progInvokeInput = Nothing,
    progInvokeInputEncoding  = IOEncodingText,
    progInvokeOutputEncoding = IOEncodingText
  }

simpleProgramInvocation :: FilePath -> [String] -> ProgramInvocation
simpleProgramInvocation path args =
  emptyProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args
  }

programInvocation :: ConfiguredProgram -> [String] -> ProgramInvocation
programInvocation prog args =
  emptyProgramInvocation {
    progInvokePath = programPath prog,
    progInvokeArgs = programDefaultArgs prog
                  ++ args
                  ++ programOverrideArgs prog,
    progInvokeEnv  = programOverrideEnv prog
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
    progInvokeEnv   = envOverrides,
    progInvokeCwd   = mcwd,
    progInvokeInput = Nothing
  } = do
    menv <- getEffectiveEnvironment envOverrides
    exitCode <- rawSystemIOWithEnv verbosity
                                   path args
                                   mcwd menv
                                   Nothing Nothing Nothing
    when (exitCode /= ExitSuccess) $
      exitWith exitCode

runProgramInvocation verbosity
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = envOverrides,
    progInvokeCwd   = mcwd,
    progInvokeInput = Just inputStr,
    progInvokeInputEncoding = encoding
  } = do
    menv <- getEffectiveEnvironment envOverrides
    (_, errors, exitCode) <- rawSystemStdInOut verbosity
                                    path args
                                    mcwd menv
                                    (Just input) True
    when (exitCode /= ExitSuccess) $
      die errors
  where
    input = case encoding of
              IOEncodingText -> (inputStr, False)
              IOEncodingUTF8 -> (toUTF8 inputStr, True) -- use binary mode for
                                                        -- utf8


getProgramInvocationOutput :: Verbosity -> ProgramInvocation -> IO String
getProgramInvocationOutput verbosity
  ProgramInvocation {
    progInvokePath  = path,
    progInvokeArgs  = args,
    progInvokeEnv   = envOverrides,
    progInvokeCwd   = mcwd,
    progInvokeInput = minputStr,
    progInvokeOutputEncoding = encoding
  } = do
    let utf8 = case encoding of IOEncodingUTF8 -> True; _ -> False
        decode | utf8      = fromUTF8 . normaliseLineEndings
               | otherwise = id
    menv <- getEffectiveEnvironment envOverrides
    (output, errors, exitCode) <- rawSystemStdInOut verbosity
                                    path args
                                    mcwd menv
                                    input utf8
    when (exitCode /= ExitSuccess) $
      die errors
    return (decode output)
  where
    input =
      case minputStr of
        Nothing       -> Nothing
        Just inputStr -> Just $
          case encoding of
            IOEncodingText -> (inputStr, False)
            IOEncodingUTF8 -> (toUTF8 inputStr, True) -- use binary mode for utf8


-- | Return the current environment extended with the given overrides.
--
getEffectiveEnvironment :: [(String, Maybe String)]
                        -> IO (Maybe [(String, String)])
getEffectiveEnvironment []        = return Nothing
getEffectiveEnvironment overrides =
    fmap (Just . Map.toList . apply overrides . Map.fromList) getEnvironment
  where
    apply os env = foldl' (flip update) env os
    update (var, Nothing)  = Map.delete var
    update (var, Just val) = Map.insert var val

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

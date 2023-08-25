{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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
module Distribution.Simple.Program.Run
  ( ProgramInvocation (..)
  , IOEncoding (..)
  , emptyProgramInvocation
  , simpleProgramInvocation
  , programInvocation
  , multiStageProgramInvocation
  , runProgramInvocation
  , getProgramInvocationOutput
  , getProgramInvocationLBS
  , getProgramInvocationOutputAndErrors
  , getEffectiveEnvironment
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Environment
import Distribution.Simple.Errors
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils
import Distribution.Utils.Generic
import Distribution.Verbosity
import System.FilePath (searchPathSeparator)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map

-- | Represents a specific invocation of a specific program.
--
-- This is used as an intermediate type between deciding how to call a program
-- and actually doing it. This provides the opportunity to the caller to
-- adjust how the program will be called. These invocations can either be run
-- directly or turned into shell or batch scripts.
data ProgramInvocation = ProgramInvocation
  { progInvokePath :: FilePath
  , progInvokeArgs :: [String]
  , progInvokeEnv :: [(String, Maybe String)]
  , -- Extra paths to add to PATH
    progInvokePathEnv :: [FilePath]
  , progInvokeCwd :: Maybe FilePath
  , progInvokeInput :: Maybe IOData
  , progInvokeInputEncoding :: IOEncoding
  -- ^ TODO: remove this, make user decide when constructing 'progInvokeInput'.
  , progInvokeOutputEncoding :: IOEncoding
  }

data IOEncoding
  = IOEncodingText -- locale mode text
  | IOEncodingUTF8 -- always utf8

encodeToIOData :: IOEncoding -> IOData -> IOData
encodeToIOData _ iod@(IODataBinary _) = iod
encodeToIOData IOEncodingText iod@(IODataText _) = iod
encodeToIOData IOEncodingUTF8 (IODataText str) = IODataBinary (toUTF8LBS str)

emptyProgramInvocation :: ProgramInvocation
emptyProgramInvocation =
  ProgramInvocation
    { progInvokePath = ""
    , progInvokeArgs = []
    , progInvokeEnv = []
    , progInvokePathEnv = []
    , progInvokeCwd = Nothing
    , progInvokeInput = Nothing
    , progInvokeInputEncoding = IOEncodingText
    , progInvokeOutputEncoding = IOEncodingText
    }

simpleProgramInvocation :: FilePath -> [String] -> ProgramInvocation
simpleProgramInvocation path args =
  emptyProgramInvocation
    { progInvokePath = path
    , progInvokeArgs = args
    }

programInvocation :: ConfiguredProgram -> [String] -> ProgramInvocation
programInvocation prog args =
  emptyProgramInvocation
    { progInvokePath = programPath prog
    , progInvokeArgs =
        programDefaultArgs prog
          ++ args
          ++ programOverrideArgs prog
    , progInvokeEnv = programOverrideEnv prog
    }

runProgramInvocation :: Verbosity -> ProgramInvocation -> IO ()
runProgramInvocation
  verbosity
  ProgramInvocation
    { progInvokePath = path
    , progInvokeArgs = args
    , progInvokeEnv = []
    , progInvokePathEnv = []
    , progInvokeCwd = Nothing
    , progInvokeInput = Nothing
    } =
    rawSystemExit verbosity path args
runProgramInvocation
  verbosity
  ProgramInvocation
    { progInvokePath = path
    , progInvokeArgs = args
    , progInvokeEnv = envOverrides
    , progInvokePathEnv = extraPath
    , progInvokeCwd = mcwd
    , progInvokeInput = Nothing
    } = do
    pathOverride <- getExtraPathEnv envOverrides extraPath
    menv <- getEffectiveEnvironment (envOverrides ++ pathOverride)
    maybeExit $
      rawSystemIOWithEnv
        verbosity
        path
        args
        mcwd
        menv
        Nothing
        Nothing
        Nothing
runProgramInvocation
  verbosity
  ProgramInvocation
    { progInvokePath = path
    , progInvokeArgs = args
    , progInvokeEnv = envOverrides
    , progInvokePathEnv = extraPath
    , progInvokeCwd = mcwd
    , progInvokeInput = Just inputStr
    , progInvokeInputEncoding = encoding
    } = do
    pathOverride <- getExtraPathEnv envOverrides extraPath
    menv <- getEffectiveEnvironment (envOverrides ++ pathOverride)
    (_, errors, exitCode) <-
      rawSystemStdInOut
        verbosity
        path
        args
        mcwd
        menv
        (Just input)
        IODataModeBinary
    when (exitCode /= ExitSuccess) $
      dieWithException verbosity $
        RunProgramInvocationException path errors
    where
      input = encodeToIOData encoding inputStr

getProgramInvocationOutput :: Verbosity -> ProgramInvocation -> IO String
getProgramInvocationOutput verbosity inv = do
  (output, errors, exitCode) <- getProgramInvocationOutputAndErrors verbosity inv
  when (exitCode /= ExitSuccess) $
    die' verbosity $
      "'" ++ progInvokePath inv ++ "' exited with an error:\n" ++ errors
  return output

getProgramInvocationLBS :: Verbosity -> ProgramInvocation -> IO LBS.ByteString
getProgramInvocationLBS verbosity inv = do
  (output, errors, exitCode) <- getProgramInvocationIODataAndErrors verbosity inv IODataModeBinary
  when (exitCode /= ExitSuccess) $
    dieWithException verbosity $
      GetProgramInvocationLBSException (progInvokePath inv) errors
  return output

getProgramInvocationOutputAndErrors
  :: Verbosity
  -> ProgramInvocation
  -> IO (String, String, ExitCode)
getProgramInvocationOutputAndErrors verbosity inv = case progInvokeOutputEncoding inv of
  IOEncodingText -> do
    (output, errors, exitCode) <- getProgramInvocationIODataAndErrors verbosity inv IODataModeText
    return (output, errors, exitCode)
  IOEncodingUTF8 -> do
    (output', errors, exitCode) <- getProgramInvocationIODataAndErrors verbosity inv IODataModeBinary
    return (normaliseLineEndings (fromUTF8LBS output'), errors, exitCode)

getProgramInvocationIODataAndErrors
  :: KnownIODataMode mode
  => Verbosity
  -> ProgramInvocation
  -> IODataMode mode
  -> IO (mode, String, ExitCode)
getProgramInvocationIODataAndErrors
  verbosity
  ProgramInvocation
    { progInvokePath = path
    , progInvokeArgs = args
    , progInvokeEnv = envOverrides
    , progInvokePathEnv = extraPath
    , progInvokeCwd = mcwd
    , progInvokeInput = minputStr
    , progInvokeInputEncoding = encoding
    }
  mode = do
    pathOverride <- getExtraPathEnv envOverrides extraPath
    menv <- getEffectiveEnvironment (envOverrides ++ pathOverride)
    rawSystemStdInOut verbosity path args mcwd menv input mode
    where
      input = encodeToIOData encoding <$> minputStr

getExtraPathEnv :: [(String, Maybe String)] -> [FilePath] -> IO [(String, Maybe String)]
getExtraPathEnv _ [] = return []
getExtraPathEnv env extras = do
  mb_path <- case lookup "PATH" env of
    Just x -> return x
    Nothing -> lookupEnv "PATH"
  let extra = intercalate [searchPathSeparator] extras
      path' = case mb_path of
        Nothing -> extra
        Just path -> extra ++ searchPathSeparator : path
  return [("PATH", Just path')]

-- | Return the current environment extended with the given overrides.
-- If an entry is specified twice in @overrides@, the second entry takes
-- precedence.
getEffectiveEnvironment
  :: [(String, Maybe String)]
  -> IO (Maybe [(String, String)])
getEffectiveEnvironment [] = return Nothing
getEffectiveEnvironment overrides =
  fmap (Just . Map.toList . apply overrides . Map.fromList) getEnvironment
  where
    apply os env = foldl' (flip update) env os
    update (var, Nothing) = Map.delete var
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
multiStageProgramInvocation
  :: ProgramInvocation
  -> (ProgramInvocation, ProgramInvocation, ProgramInvocation)
  -> [String]
  -> [ProgramInvocation]
multiStageProgramInvocation simple (initial, middle, final) args =
  let argSize inv =
        length (progInvokePath inv)
          + foldl' (\s a -> length a + 1 + s) 1 (progInvokeArgs inv)
      fixedArgSize = maximum (map argSize [simple, initial, middle, final])
      chunkSize = maxCommandLineSize - fixedArgSize
   in case splitChunks chunkSize args of
        [] -> [simple]
        [c] -> [simple `appendArgs` c]
        (c : c2 : cs)
          | (xs, x) <- unsnocNE (c2 :| cs) ->
              [initial `appendArgs` c]
                ++ [middle `appendArgs` c' | c' <- xs]
                ++ [final `appendArgs` x]
  where
    appendArgs :: ProgramInvocation -> [String] -> ProgramInvocation
    inv `appendArgs` as = inv{progInvokeArgs = progInvokeArgs inv ++ as}

    splitChunks :: Int -> [[a]] -> [[[a]]]
    splitChunks len = unfoldr $ \s ->
      if null s
        then Nothing
        else Just (chunk len s)

    chunk :: Int -> [[a]] -> ([[a]], [[a]])
    chunk len (s : _) | length s >= len = error toolong
    chunk len ss = chunk' [] len ss

    chunk' :: [[a]] -> Int -> [[a]] -> ([[a]], [[a]])
    chunk' acc len (s : ss)
      | len' < len = chunk' (s : acc) (len - len' - 1) ss
      where
        len' = length s
    chunk' acc _ ss = (reverse acc, ss)

    toolong =
      "multiStageProgramInvocation: a single program arg is larger "
        ++ "than the maximum command line length!"

-- FIXME: discover this at configure time or runtime on unix
-- The value is 32k on Windows and posix specifies a minimum of 4k
-- but all sensible unixes use more than 4k.
-- we could use getSysVar ArgumentLimit but that's in the unix lib
--
maxCommandLineSize :: Int
maxCommandLineSize = 30 * 1024

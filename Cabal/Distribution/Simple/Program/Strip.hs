-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Strip
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @strip@ program.

module Distribution.Simple.Program.Strip (stripLib, stripExe)
       where

import Distribution.Simple.Program (ProgramConfiguration, lookupProgram
                                   ,rawSystemProgram, stripProgram)
import Distribution.Simple.Utils   (warn)
import Distribution.System         (Platform(..), OS (..), buildOS)
import Distribution.Verbosity      (Verbosity)

import Control.Monad               (unless)
import System.FilePath             (takeBaseName)

runStrip :: Verbosity -> ProgramConfiguration -> FilePath -> [String] -> IO ()
runStrip verbosity progConf path args =
  case lookupProgram stripProgram progConf of
    Just strip -> rawSystemProgram verbosity strip (path:args)
    Nothing    -> unless (buildOS == Windows) $
                  -- Don't bother warning on windows, we don't expect them to
                  -- have the strip program anyway.
                  warn verbosity $ "Unable to strip executable or library '"
                                   ++ (takeBaseName path)
                                   ++ "' (missing the 'strip' program)"

stripExe :: Verbosity -> Platform -> ProgramConfiguration -> FilePath -> IO ()
stripExe verbosity (Platform _arch os) conf path =
  runStrip verbosity conf path args
  where
    args = case os of
       OSX -> ["-x"] -- By default, stripping the ghc binary on at least
                     -- some OS X installations causes:
                     --     HSbase-3.0.o: unknown symbol `_environ'"
                     -- The -x flag fixes that.
       _   -> []

stripLib :: Verbosity -> Platform -> ProgramConfiguration -> FilePath -> IO ()
stripLib verbosity (Platform _arch os) conf path = do
  case os of
    OSX -> -- '--strip-unneeded' is not supported on OS X or iOS. See #1630.
           return ()
    IOS -> return ()
    _   -> runStrip verbosity conf path args
  where
    args = ["--strip-unneeded"]

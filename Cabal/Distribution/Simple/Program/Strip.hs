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
                                   , programVersion, rawSystemProgram
                                   , stripProgram)
import Distribution.Simple.Utils   (warn)
import Distribution.System         (Arch(..), Platform(..), OS (..), buildOS)
import Distribution.Verbosity      (Verbosity)
import Distribution.Version        (orLaterVersion, withinRange)

import Control.Monad               (unless)
import Data.Version                (Version(..))
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
stripLib verbosity (Platform arch os) conf path = do
  case os of
    OSX -> -- '--strip-unneeded' is not supported on OS X, iOS, AIX, or
           -- Solaris. See #1630.
           return ()
    IOS -> return ()
    AIX -> return ()
    Solaris -> return ()
    Windows -> -- Stripping triggers a bug in 'strip.exe' for
               -- libraries with lots identically named modules. See
               -- #1784.
               return()
    Linux | arch == I386 ->
      -- Versions of 'strip' on 32-bit Linux older than 2.18 are
      -- broken. See #2339.
      let okVersion = orLaterVersion (Version [2,18] [])
      in case programVersion =<< lookupProgram stripProgram conf of
          Just v | withinRange v okVersion ->
            runStrip verbosity conf path args
          _ -> warn verbosity $ "Unable to strip library '"
                                ++ (takeBaseName path)
                                ++ "' (version of 'strip' too old; "
                                ++ "requires >= 2.18 on 32-bit Linux)"
    _   -> runStrip verbosity conf path args
  where
    args = ["--strip-unneeded"]

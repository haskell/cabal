-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Ar
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ar@ program.

module Distribution.Simple.Program.Ar (
    createArLibArchive,
    multiStageProgramInvocation,
  ) where

import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..) )
import Distribution.Simple.Program.Run
         ( programInvocation, multiStageProgramInvocation
         , runProgramInvocation )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Verbosity
         ( Verbosity, deafening, verbose )

-- | Call @ar@ to create a library archive from a bunch of object files.
--
createArLibArchive :: Verbosity -> ConfiguredProgram
                   -> FilePath -> [FilePath] -> IO ()
createArLibArchive verbosity ar target files =

  -- The args to use with "ar" are actually rather subtle and system-dependent.
  -- In particular we have the following issues:
  --
  --  -- On OS X, "ar q" does not make an archive index. Archives with no
  --     index cannot be used.
  --
  --  -- GNU "ar r" will not let us add duplicate objects, only "ar q" lets us
  --     do that. We have duplicates because of modules like "A.M" and "B.M"
  --     both make an object file "M.o" and ar does not consider the directory.
  --
  -- Our solution is to use "ar r" in the simple case when one call is enough.
  -- When we need to call ar multiple times we use "ar q" and for the last
  -- call on OSX we use "ar qs" so that it'll make the index.

  let simpleArgs  = case buildOS of
             OSX -> ["-r", "-s"]
             _   -> ["-r"]

      initialArgs = ["-q"]
      finalArgs   = case buildOS of
             OSX -> ["-q", "-s"]
             _   -> ["-q"]

      extraArgs   = verbosityOpts verbosity ++ [target]

      simple  = programInvocation ar (simpleArgs  ++ extraArgs)
      initial = programInvocation ar (initialArgs ++ extraArgs)
      middle  = initial
      final   = programInvocation ar (finalArgs   ++ extraArgs)

   in sequence_
        [ runProgramInvocation verbosity inv
        | inv <- multiStageProgramInvocation
                   simple (initial, middle, final) files ]

  where
    verbosityOpts v | v >= deafening = ["-v"]
                    | v >= verbose   = []
                    | otherwise      = ["-c"]

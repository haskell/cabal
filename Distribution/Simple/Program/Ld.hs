-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Ld
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ld@ linker program.

module Distribution.Simple.Program.Ld (
    combineObjectFiles,
  ) where

import Distribution.Simple.Program.Types
         ( ConfiguredProgram(..) )
import Distribution.Simple.Program.Run
         ( programInvocation, multiStageProgramInvocation
         , runProgramInvocation )
import Distribution.Verbosity
         ( Verbosity )

import System.Directory
         ( renameFile )
import System.FilePath
         ( (<.>) )

-- | Call @ld -r@ to link a bunch of object files together.
--
combineObjectFiles :: Verbosity -> ConfiguredProgram
                   -> FilePath -> [FilePath] -> IO ()
combineObjectFiles verbosity ld target files =

  -- Unlike "ar", the "ld" tool is not designed to be used with xargs. That is,
  -- if we have more object files than fit on a single command line then we
  -- have a slight problem. What we have to do is link files in batches into
  -- a temp object file and then include that one in the next batch.

  let simpleArgs  = ["-r", "-o", target]

      initialArgs = ["-r", "-o", target]
      middleArgs  = ["-r", "-o", target, tmpfile]
      finalArgs   = middleArgs

      simple      = programInvocation ld simpleArgs
      initial     = programInvocation ld initialArgs
      middle      = programInvocation ld middleArgs
      final       = programInvocation ld finalArgs

      invocations = multiStageProgramInvocation
                      simple (initial, middle, final) files

   in run invocations

  where
    tmpfile        = target <.> "tmp" -- perhaps should use a proper temp file

    run []         = return ()
    run [inv]      = runProgramInvocation verbosity inv
    run (inv:invs) = do runProgramInvocation verbosity inv
                        renameFile target tmpfile
                        run invs

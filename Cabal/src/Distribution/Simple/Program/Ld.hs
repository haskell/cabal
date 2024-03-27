{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Program.Ld
-- Copyright   :  Duncan Coutts 2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @ld@ linker program.
module Distribution.Simple.Program.Ld
  ( combineObjectFiles
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Compiler (arResponseFilesSupported)
import Distribution.Simple.Flag
  ( fromFlagOrDefault
  )
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..), mbWorkDirLBI)
import Distribution.Simple.Program.ResponseFile
  ( withResponseFile
  )
import Distribution.Simple.Program.Run
  ( ProgramInvocation
  , multiStageProgramInvocation
  , programInvocationCwd
  , runProgramInvocation
  )
import Distribution.Simple.Program.Types
  ( ConfiguredProgram (..)
  )
import Distribution.Simple.Setup.Config
  ( configUseResponseFiles
  )
import Distribution.Simple.Utils
  ( defaultTempFileOptions
  )
import Distribution.Utils.Path
import Distribution.Verbosity
  ( Verbosity
  )

import System.Directory
  ( renameFile
  )

-- | Call @ld -r@ to link a bunch of object files together.
combineObjectFiles
  :: Verbosity
  -> LocalBuildInfo
  -> ConfiguredProgram
  -> SymbolicPath Pkg File
  -> [SymbolicPath Pkg File]
  -> IO ()
combineObjectFiles verbosity lbi ldProg target files = do
  -- Unlike "ar", the "ld" tool is not designed to be used with xargs. That is,
  -- if we have more object files than fit on a single command line then we
  -- have a slight problem. What we have to do is link files in batches into
  -- a temp object file and then include that one in the next batch.

  let
    -- See Note [Symbolic paths] in Distribution.Utils.Path
    u :: SymbolicPath Pkg to -> FilePath
    u = interpretSymbolicPathCWD
    i = interpretSymbolicPath mbWorkDir
    mbWorkDir = mbWorkDirLBI lbi

    simpleArgs = ["-r", "-o", u target]
    initialArgs = ["-r", "-o", u target]
    middleArgs = ["-r", "-o", u target, u tmpfile]
    finalArgs = middleArgs

    ld = programInvocationCwd (mbWorkDirLBI lbi) ldProg
    simple = ld simpleArgs
    initial = ld initialArgs
    middle = ld middleArgs
    final = ld finalArgs

    targetDir = takeDirectorySymbolicPath target

    invokeWithResponseFile :: FilePath -> ProgramInvocation
    invokeWithResponseFile atFile =
      ld $ simpleArgs ++ ['@' : atFile]

    oldVersionManualOverride =
      fromFlagOrDefault False $ configUseResponseFiles $ configFlags lbi
    -- Whether ghc's ar supports response files is a good proxy for
    -- whether ghc's ld supports them as well.
    responseArgumentsNotSupported =
      not (arResponseFilesSupported (compiler lbi))

    run :: [ProgramInvocation] -> IO ()
    run [] = return ()
    run [inv] = runProgramInvocation verbosity inv
    run (inv : invs) = do
      runProgramInvocation verbosity inv
      renameFile (i target) (i tmpfile)
      run invs

  if oldVersionManualOverride || responseArgumentsNotSupported
    then run $ multiStageProgramInvocation simple (initial, middle, final) (map getSymbolicPath files)
    else withResponseFile verbosity defaultTempFileOptions mbWorkDir targetDir "ld.rsp" Nothing (map getSymbolicPath files) $
      \path -> runProgramInvocation verbosity $ invokeWithResponseFile path
  where
    tmpfile = target <.> "tmp" -- perhaps should use a proper temp file

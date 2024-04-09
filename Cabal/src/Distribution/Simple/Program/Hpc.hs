{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Program.Hpc
-- Copyright   :  Thomas Tuegel 2011
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides an library interface to the @hpc@ program.
module Distribution.Simple.Program.Hpc
  ( markup
  , union
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName
import Distribution.Pretty
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version

-- | Invoke hpc with the given parameters.
--
-- Prior to HPC version 0.7 (packaged with GHC 7.8), hpc did not handle
-- multiple .mix paths correctly, so we print a warning, and only pass it the
-- first path in the list. This means that e.g. test suites that import their
-- library as a dependency can still work, but those that include the library
-- modules directly (in other-modules) don't.
markup
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> ConfiguredProgram
  -> Version
  -> Verbosity
  -> SymbolicPath Pkg File
  -- ^ Path to .tix file
  -> [SymbolicPath Pkg (Dir Mix)]
  -- ^ Paths to .mix file directories
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ Path where html output should be located
  -> [ModuleName]
  -- ^ List of modules to include in the report
  -> IO ()
markup mbWorkDir hpc hpcVer verbosity tixFile hpcDirs destDir included = do
  hpcDirs' <-
    if withinRange hpcVer (orLaterVersion version07)
      then return hpcDirs
      else do
        warn verbosity $
          "Your version of HPC ("
            ++ prettyShow hpcVer
            ++ ") does not properly handle multiple search paths. "
            ++ "Coverage report generation may fail unexpectedly. These "
            ++ "issues are addressed in version 0.7 or later (GHC 7.8 or "
            ++ "later)."
            ++ if null droppedDirs
              then ""
              else
                " The following search paths have been abandoned: "
                  ++ show droppedDirs
        return passedDirs

  -- Prior to GHC 8.0, hpc assumes all .mix paths are relative.
  hpcDirs'' <- traverse (tryMakeRelativeToWorkingDir mbWorkDir) hpcDirs'

  runProgramInvocation
    verbosity
    (markupInvocation mbWorkDir hpc tixFile hpcDirs'' destDir included)
  where
    version07 = mkVersion [0, 7]
    (passedDirs, droppedDirs) = splitAt 1 hpcDirs

markupInvocation
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> ConfiguredProgram
  -> SymbolicPath Pkg File
  -- ^ Path to .tix file
  -> [SymbolicPath Pkg (Dir Mix)]
  -- ^ Paths to .mix file directories
  -> SymbolicPath Pkg (Dir Artifacts)
  -- ^ Path where html output should be
  -- located
  -> [ModuleName]
  -- ^ List of modules to include
  -> ProgramInvocation
markupInvocation mbWorkDir hpc tixFile hpcDirs destDir included =
  let args =
        [ "markup"
        , getSymbolicPath tixFile
        , "--destdir=" ++ getSymbolicPath destDir
        ]
          ++ map (("--hpcdir=" ++) . getSymbolicPath) hpcDirs
          ++ [ "--include=" ++ prettyShow moduleName
             | moduleName <- included
             ]
   in programInvocationCwd mbWorkDir hpc args

union
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> ConfiguredProgram
  -> Verbosity
  -> [SymbolicPath Pkg File]
  -- ^ Paths to .tix files
  -> SymbolicPath Pkg File
  -- ^ Path to resultant .tix file
  -> [ModuleName]
  -- ^ List of modules to exclude from union
  -> IO ()
union mbWorkDir hpc verbosity tixFiles outFile excluded =
  runProgramInvocation
    verbosity
    (unionInvocation mbWorkDir hpc tixFiles outFile excluded)

unionInvocation
  :: Maybe (SymbolicPath CWD (Dir Pkg))
  -> ConfiguredProgram
  -> [SymbolicPath Pkg File]
  -- ^ Paths to .tix files
  -> SymbolicPath Pkg File
  -- ^ Path to resultant .tix file
  -> [ModuleName]
  -- ^ List of modules to exclude from union
  -> ProgramInvocation
unionInvocation mbWorkDir hpc tixFiles outFile excluded =
  programInvocationCwd mbWorkDir hpc $
    concat
      [ ["sum", "--union"]
      , map getSymbolicPath tixFiles
      , ["--output=" ++ getSymbolicPath outFile]
      , [ "--exclude=" ++ prettyShow moduleName
        | moduleName <- excluded
        ]
      ]

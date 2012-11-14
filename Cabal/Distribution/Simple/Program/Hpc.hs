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

import Control.Monad ( unless )
import Distribution.ModuleName ( ModuleName )
import Distribution.Simple.Program.Run
         ( ProgramInvocation, programInvocation, runProgramInvocation )
import Distribution.Simple.Program.Types ( ConfiguredProgram(..) )
import Distribution.Text ( display )
import Distribution.Simple.Utils ( warn )
import Distribution.Verbosity ( Verbosity )
import Distribution.Version ( Version(..), orLaterVersion, withinRange )

markup :: ConfiguredProgram
       -> Verbosity
       -> FilePath            -- ^ Path to .tix file
       -> [FilePath]          -- ^ Paths to .mix file directories
       -> FilePath            -- ^ Path where html output should be located
       -> [ModuleName]        -- ^ List of modules to exclude from report
       -> IO ()
markup hpc verbosity tixFile hpcDirs destDir excluded = do
    unless atLeastHpc07 $ warn verbosity $
           "This version of HPC has known issues. Coverage report generation "
        ++ "may fail unexpectedly. Please upgrade to HPC 0.7 or later "
        ++ "(GHC 7.8 or later) as soon as possible."
        ++ versionMsg
    runProgramInvocation verbosity
      (markupInvocation hpc tixFile hpcDirs' destDir excluded)
  where
    hpcDirs' | atLeastHpc07 = hpcDirs
             | otherwise = take 1 hpcDirs
    atLeastHpc07 = maybe False (flip withinRange $ orLaterVersion version07)
        $ programVersion hpc
    version07 = Version { versionBranch = [0, 7], versionTags = [] }
    versionMsg = maybe "" (\v -> " (Found HPC " ++ display v ++ ")")
                          (programVersion hpc)

markupInvocation :: ConfiguredProgram
                 -> FilePath            -- ^ Path to .tix file
                 -> [FilePath]          -- ^ Paths to .mix file directories
                 -> FilePath            -- ^ Path where html output should be
                                        -- located
                 -> [ModuleName]        -- ^ List of modules to exclude from
                                        -- report
                 -> ProgramInvocation
markupInvocation hpc tixFile hpcDirs destDir excluded =
    let args = [ "markup", tixFile
               , "--destdir=" ++ destDir
               ]
            ++ map ("--hpcdir=" ++) hpcDirs
            ++ ["--exclude=" ++ display moduleName
               | moduleName <- excluded ]
    in programInvocation hpc args

union :: ConfiguredProgram
      -> Verbosity
      -> [FilePath]         -- ^ Paths to .tix files
      -> FilePath           -- ^ Path to resultant .tix file
      -> [ModuleName]       -- ^ List of modules to exclude from union
      -> IO ()
union hpc verbosity tixFiles outFile excluded =
    runProgramInvocation verbosity
      (unionInvocation hpc tixFiles outFile excluded)

unionInvocation :: ConfiguredProgram
                -> [FilePath]       -- ^ Paths to .tix files
                -> FilePath         -- ^ Path to resultant .tix file
                -> [ModuleName]     -- ^ List of modules to exclude from union
                -> ProgramInvocation
unionInvocation hpc tixFiles outFile excluded =
    programInvocation hpc $ concat
        [ ["sum", "--union"]
        , tixFiles
        , ["--output=" ++ outFile]
        , ["--exclude=" ++ display moduleName
          | moduleName <- excluded ]
        ]

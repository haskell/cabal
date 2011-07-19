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

import Distribution.ModuleName ( ModuleName )
import Distribution.Simple.Program ( getProgramInvocationOutput )
import Distribution.Simple.Program.Run ( ProgramInvocation, programInvocation )
import Distribution.Simple.Program.Types ( ConfiguredProgram )
import Distribution.Text ( display )
import Distribution.Verbosity ( Verbosity )

markup :: ConfiguredProgram
       -> Verbosity
       -> FilePath            -- ^ Path to .tix file
       -> FilePath            -- ^ Path to directory with .mix files
       -> FilePath            -- ^ Path where html output should be located
       -> [ModuleName]        -- ^ List of modules to exclude from report
       -> IO ()
markup hpc verbosity tixFile hpcDir destDir excluded = do
    _ <- getProgramInvocationOutput verbosity
        (markupInvocation hpc tixFile hpcDir destDir excluded)
    return ()

markupInvocation :: ConfiguredProgram
                 -> FilePath            -- ^ Path to .tix file
                 -> FilePath            -- ^ Path to directory with .mix files
                 -> FilePath            -- ^ Path where html output should be
                                        -- located
                 -> [ModuleName]        -- ^ List of modules to exclude from
                                        -- report
                 -> ProgramInvocation
markupInvocation hpc tixFile hpcDir destDir excluded =
    let args = [ "markup", tixFile
               , "--hpcdir=" ++ hpcDir
               , "--destdir=" ++ destDir
               ] ++ exclude excluded
    in programInvocation hpc args

union :: ConfiguredProgram
      -> Verbosity
      -> [FilePath]         -- ^ Paths to .tix files
      -> FilePath           -- ^ Path to resultant .tix file
      -> [ModuleName]       -- ^ List of modules to exclude from union
      -> IO ()
union hpc verbosity tixFiles outFile excluded = do
    _ <- getProgramInvocationOutput verbosity
        $ unionInvocation hpc tixFiles outFile excluded
    return ()

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
        , exclude excluded
        ]

-- | Turn a list of modules to be excluded from coverage results into a list
-- of command line options to hpc.
exclude :: [ModuleName] -> [String]
exclude = map (("--exclude=" ++) . display)

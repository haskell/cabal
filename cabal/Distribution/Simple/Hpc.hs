-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Hpc
-- Copyright   :  Thomas Tuegel 2011
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides functions for locating various HPC-related paths and
-- a function for adding the necessary options to a PackageDescription to
-- build test suites with HPC enabled.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Hpc
    ( enableCoverage
    , htmlDir
    , tixDir
    , tixFilePath
    , markupPackage
    , markupTest
    ) where

import Control.Monad ( when )
import Distribution.Compiler ( CompilerFlavor(..) )
import Distribution.ModuleName ( main )
import Distribution.PackageDescription
    ( BuildInfo(..)
    , Library(..)
    , PackageDescription(..)
    , TestSuite(..)
    , testModules
    )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo(..) )
import Distribution.Simple.Program ( hpcProgram, requireProgram )
import Distribution.Simple.Program.Hpc ( markup, union )
import Distribution.Simple.Utils ( notice )
import Distribution.Text
import Distribution.Verbosity ( Verbosity() )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath

-- -------------------------------------------------------------------------
-- Haskell Program Coverage

-- | Conditionally enable Haskell Program Coverage by adding the necessary
-- GHC options to a PackageDescription.
--
-- TODO: do this differently in the build stage by constructing local build
-- info, not by modifying the original PackageDescription.
--
enableCoverage :: Bool                  -- ^ Enable coverage?
               -> String                -- ^ \"dist/\" prefix
               -> PackageDescription
               -> PackageDescription
enableCoverage False _ x = x
enableCoverage True distPref p =
    p { library = fmap enableLibCoverage (library p)
      , testSuites = map enableTestCoverage (testSuites p)
      }
  where
    enableBICoverage name oldBI =
        let oldOptions = options oldBI
            oldGHCOpts = lookup GHC oldOptions
            newGHCOpts = case oldGHCOpts of
                             Just xs -> (GHC, hpcOpts ++ xs)
                             _ -> (GHC, hpcOpts)
            newOptions = (:) newGHCOpts $ filter ((== GHC) . fst) oldOptions
            hpcOpts = ["-fhpc", "-hpcdir", mixDir distPref name]
        in oldBI { options = newOptions }
    enableLibCoverage l =
        l { libBuildInfo = enableBICoverage (display $ package p)
                                            (libBuildInfo l)
          }
    enableTestCoverage t =
        t { testBuildInfo = enableBICoverage (testName t) (testBuildInfo t) }

hpcDir :: FilePath  -- ^ \"dist/\" prefix
       -> FilePath  -- ^ Directory containing component's HPC .mix files
hpcDir distPref = distPref </> "hpc"

mixDir :: FilePath  -- ^ \"dist/\" prefix
       -> FilePath  -- ^ Component name
       -> FilePath  -- ^ Directory containing test suite's .mix files
mixDir distPref name = hpcDir distPref </> "mix" </> name

tixDir :: FilePath  -- ^ \"dist/\" prefix
       -> FilePath  -- ^ Component name
       -> FilePath  -- ^ Directory containing test suite's .tix files
tixDir distPref name = hpcDir distPref </> "tix" </> name

-- | Path to the .tix file containing a test suite's sum statistics.
tixFilePath :: FilePath     -- ^ \"dist/\" prefix
            -> FilePath     -- ^ Component name
            -> FilePath     -- ^ Path to test suite's .tix file
tixFilePath distPref name = tixDir distPref name </> name <.> "tix"

htmlDir :: FilePath     -- ^ \"dist/\" prefix
        -> FilePath     -- ^ Component name
        -> FilePath     -- ^ Path to test suite's HTML markup directory
htmlDir distPref name = hpcDir distPref </> "html" </> name

-- | Generate the HTML markup for a test suite.
markupTest :: Verbosity
           -> LocalBuildInfo
           -> FilePath     -- ^ \"dist/\" prefix
           -> String       -- ^ Library name
           -> TestSuite
           -> IO ()
markupTest verbosity lbi distPref libName suite = do
    tixFileExists <- doesFileExist $ tixFilePath distPref $ testName suite
    when tixFileExists $ do
        (hpc, _) <- requireProgram verbosity hpcProgram $ withPrograms lbi
        markup hpc verbosity (tixFilePath distPref $ testName suite)
                             (mixDir distPref libName)
                             (htmlDir distPref $ testName suite)
                             (testModules suite ++ [ main ])
        notice verbosity $ "Test coverage report written to "
                            ++ htmlDir distPref (testName suite)
                            </> "hpc_index" <.> "html"

-- | Generate the HTML markup for all of a package's test suites.
markupPackage :: Verbosity
              -> LocalBuildInfo
              -> FilePath       -- ^ \"dist/\" prefix
              -> String         -- ^ Library name
              -> [TestSuite]
              -> IO ()
markupPackage verbosity lbi distPref libName suites = do
    let tixFiles = map (tixFilePath distPref . testName) suites
    tixFilesExist <- mapM doesFileExist tixFiles
    when (and tixFilesExist) $ do
        (hpc, _) <- requireProgram verbosity hpcProgram $ withPrograms lbi
        let outFile = tixFilePath distPref libName
            mixDir' = mixDir distPref libName
            htmlDir' = htmlDir distPref libName
            excluded = concatMap testModules suites ++ [ main ]
        createDirectoryIfMissing True $ takeDirectory outFile
        union hpc verbosity tixFiles outFile excluded
        markup hpc verbosity outFile mixDir' htmlDir' excluded
        notice verbosity $ "Package coverage report written to "
                           ++ htmlDir' </> "hpc_index.html"

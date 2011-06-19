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
    ( hpcDir
    , enableCoverage
    , tixDir
    , tixFilePath
    , doHpcMarkup
    , findTixFiles
    ) where

import Control.Exception ( bracket )
import Control.Monad ( unless, when )
import Distribution.Compiler ( CompilerFlavor(..) )
import Distribution.ModuleName ( main )
import Distribution.PackageDescription
    ( BuildInfo(..)
    , Library(..)
    , PackageDescription(..)
    , TestSuite(..)
    , testModules
    )
import Distribution.Simple.Utils ( die, notice )
import Distribution.Text
import Distribution.Verbosity ( Verbosity() )
import System.Directory ( doesFileExist, getDirectoryContents, removeFile )
import System.Exit ( ExitCode(..) )
import System.FilePath
import System.IO ( hClose, IOMode(..), openFile, openTempFile )
import System.Process ( runProcess, waitForProcess )

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
            hpcOpts = ["-fhpc", "-hpcdir", hpcDir distPref name]
        in oldBI { options = newOptions }
    enableLibCoverage l =
        l { libBuildInfo = enableBICoverage (display $ package p)
                                            (libBuildInfo l)
          }
    enableTestCoverage t =
        t { testBuildInfo = enableBICoverage (testName t) (testBuildInfo t) }

hpcDir :: FilePath  -- ^ \"dist/\" prefix
       -> FilePath  -- ^ Component subdirectory name
       -> FilePath  -- ^ Directory containing component's HPC .mix files
hpcDir distPref name = distPref </> "hpc" </> name

tixDir :: FilePath  -- ^ \"dist/\" prefix
       -> TestSuite -- ^ Test suite
       -> FilePath  -- ^ Directory containing test suite's .tix files
tixDir distPref suite = distPref </> "test" </> testName suite

-- | Path to the .tix file containing a test suite's sum statistics.
tixFilePath :: FilePath     -- ^ \"dist/\" prefix
            -> TestSuite    -- ^ Test suite
            -> FilePath     -- Path to test suite's .tix file
tixFilePath distPref suite = tixDir distPref suite </> testName suite <.> "tix"

-- | Returns a list of all the .tix files in a test suite's .tix file
-- directory. Returned paths are the complete relative path to each file.
findTixFiles :: FilePath        -- ^ \"dist/\" prefix
             -> TestSuite       -- ^ Test suite
             -> IO [FilePath]   -- ^ All .tix files belonging to test suite
findTixFiles distPref suite = do
    files <- getDirectoryContents $ tixDir distPref suite
    let tixFiles = flip filter files $ \x -> takeExtension x == ".tix"
    return $ map (tixDir distPref suite </>) tixFiles

-- | Generate the HTML markup for a test suite.
doHpcMarkup :: Verbosity
            -> FilePath     -- ^ \"dist/\" prefix
            -> String       -- ^ Library name
            -> TestSuite
            -> IO ()
doHpcMarkup verbosity distPref libName suite = do
    tixFiles <- findTixFiles distPref suite
    when (not $ null tixFiles) $ do
        let hpcOptions = map (\x -> "--exclude=" ++ display x) excluded
            unionOptions = [ "sum"
                           , "--union"
                           , "--output=" ++ tixFilePath distPref suite
                           ]
                           ++ hpcOptions ++ tixFiles
            markupOptions = [ "markup"
                            , tixFilePath distPref suite
                            , "--hpcdir=" ++ hpcDir distPref libName
                            , "--destdir=" ++ tixDir distPref suite
                            ]
                            ++ hpcOptions
            excluded = testModules suite ++ [ main ]
            --TODO: use standard process utilities from D.S.Utils
            runHpc opts h = runProcess "hpc" opts Nothing Nothing Nothing
                                       (Just h) (Just h)
        bracket (openHpcTemp $ tixDir distPref suite) deleteIfExists
            $ \hpcOut -> do
            hUnion <- openFile hpcOut AppendMode
            procUnion <- runHpc unionOptions hUnion
            exitUnion <- waitForProcess procUnion
            success <- case exitUnion of
                ExitSuccess -> do
                    hMarkup <- openFile hpcOut AppendMode
                    procMarkup <- runHpc markupOptions hMarkup
                    exitMarkup <- waitForProcess procMarkup
                    case exitMarkup of
                        ExitSuccess -> return True
                        _ -> return False
                _ -> return False
            unless success $ do
                errs <- readFile hpcOut
                die $ "HPC failed:\n" ++ errs
            when success $ notice verbosity
                $ "Test coverage report written to "
                  ++ tixDir distPref suite </> "hpc_index"
                  <.> "html"
            return ()
  where openHpcTemp dir = do
            (f, h) <- openTempFile dir $ "cabal-test-hpc-" <.> "log"
            hClose h >> return f
        deleteIfExists path = do
            exists <- doesFileExist path
            when exists $ removeFile path

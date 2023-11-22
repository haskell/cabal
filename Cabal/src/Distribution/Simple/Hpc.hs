{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Hpc
-- Copyright   :  Thomas Tuegel 2011
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module provides functions for locating various HPC-related paths and
-- a function for adding the necessary options to a PackageDescription to
-- build test suites with HPC enabled.
module Distribution.Simple.Hpc
  ( Way (..)
  , guessWay
  , htmlDir
  , mixDir
  , tixDir
  , tixFilePath
  , markupPackage
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (main)
import Distribution.PackageDescription
  ( TestSuite (..)
  , testModules
  )
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program
  ( hpcProgram
  , requireProgramVersion
  )
import Distribution.Simple.Program.Hpc (markup, union)
import Distribution.Simple.Setup (TestFlags (..))
import Distribution.Simple.Utils (notice)
import Distribution.Types.UnqualComponentName
import Distribution.Verbosity (Verbosity ())
import Distribution.Version (anyVersion)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath

-- -------------------------------------------------------------------------
-- Haskell Program Coverage

data Way = Vanilla | Prof | Dyn
  deriving (Bounded, Enum, Eq, Read, Show)

hpcDir
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Directory containing component's HPC .mix files
hpcDir distPref way = distPref </> "hpc" </> wayDir
  where
    wayDir = case way of
      Vanilla -> "vanilla"
      Prof -> "prof"
      Dyn -> "dyn"

mixDir
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Directory containing test suite's .mix files
mixDir distPref way = hpcDir distPref way </> "mix"

tixDir
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Directory containing test suite's .tix files
tixDir distPref way = hpcDir distPref way </> "tix"

-- | Path to the .tix file containing a test suite's sum statistics.
tixFilePath
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Component name
  -> FilePath
  -- ^ Path to test suite's .tix file
tixFilePath distPref way name = tixDir distPref way </> name <.> "tix"

htmlDir
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Path to test suite's HTML markup directory
htmlDir distPref way = hpcDir distPref way </> "html"

-- | Attempt to guess the way the test suites in this package were compiled
-- and linked with the library so the correct module interfaces are found.
guessWay :: LocalBuildInfo -> Way
guessWay lbi
  | withProfExe lbi = Prof
  | withDynExe lbi = Dyn
  | otherwise = Vanilla

-- | Generate the HTML markup for a package's test suites.
markupPackage
  :: Verbosity
  -> TestFlags
  -> LocalBuildInfo
  -> FilePath
  -- ^ Testsuite \"dist/\" prefix
  -> PD.PackageDescription
  -> [TestSuite]
  -> IO ()
markupPackage verbosity TestFlags{testCoverageDistPrefs, testCoverageLibsModules} lbi testDistPref pkg_descr suites = do
  let tixFiles = map (tixFilePath testDistPref way) testNames
  tixFilesExist <- traverse doesFileExist tixFiles
  when (and tixFilesExist) $ do
    -- behaviour of 'markup' depends on version, so we need *a* version
    -- but no particular one
    (hpc, hpcVer, _) <-
      requireProgramVersion
        verbosity
        hpcProgram
        anyVersion
        (withPrograms lbi)
    let htmlDir' = htmlDir testDistPref way
    -- The tix file used to generate the report is either the testsuite's
    -- tix file, when there is only one testsuite, or the sum of the tix
    -- files of all testsuites in the package, which gets put under pkgName
    -- for this component (a bit weird)
    -- TODO: cabal-install should pass to Cabal where to put the summed tix
    -- and report, and perhaps even the testsuites from other packages in
    -- the project which are currently not accounted for in the summed
    -- report.
    tixFile <- case suites of
      -- We call 'markupPackage' once for each testsuite to run individually,
      -- to get the coverage report of just the one testsuite
      [oneTest] -> do
        let testName' = unUnqualComponentName $ testName oneTest
        return $
          tixFilePath testDistPref way testName'
      -- And call 'markupPackage' once per `test` invocation with all the
      -- testsuites to run, which results in multiple tix files being considered
      _ -> do
        let excluded = concatMap testModules suites ++ [main]
            pkgName = prettyShow $ PD.package pkg_descr
            summedTixFile = tixFilePath testDistPref way pkgName
        createDirectoryIfMissing True $ takeDirectory summedTixFile
        union hpc verbosity tixFiles summedTixFile excluded
        return summedTixFile

    markup hpc hpcVer verbosity tixFile mixDirs htmlDir' included
    notice verbosity $
      "Package coverage report written to "
        ++ htmlDir'
        </> "hpc_index.html"
  where
    way = guessWay lbi
    testNames = fmap (unUnqualComponentName . testName) suites
    mixDirs = map (`mixDir` way) (fromFlagOrDefault [] testCoverageDistPrefs)
    included = fromFlagOrDefault [] testCoverageLibsModules

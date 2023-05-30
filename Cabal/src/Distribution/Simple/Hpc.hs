{-# LANGUAGE FlexibleContexts #-}
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
  , markupTest
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (main)
import Distribution.PackageDescription
  ( Library (..)
  , TestSuite (..)
  , testModules
  )
import qualified Distribution.PackageDescription as PD
import Distribution.Pretty
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (..))
import Distribution.Simple.Program
  ( hpcProgram
  , requireProgramVersion
  )
import Distribution.Simple.Program.Hpc (markup, union)
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
  -- ^ Component name
  -> FilePath
  -- ^ Directory containing test suite's .mix files
mixDir distPref way name = hpcDir distPrefBuild way </> "mix" </> name
  where
    -- This is a hack for HPC over test suites, needed to match the directory
    -- where HPC saves and reads .mix files when the main library of the same
    -- package is being processed, perhaps in a previous cabal run (#5213).
    -- E.g., @distPref@ may be
    -- @./dist-newstyle/build/x86_64-linux/ghc-9.0.1/cabal-gh5213-0.1/t/tests@
    -- but the path where library mix files reside has two less components
    -- at the end (@t/tests@) and this reduced path needs to be passed to
    -- both @hpc@ and @ghc@. For non-default optimization levels, the path
    -- suffix is one element longer and the extra path element needs
    -- to be preserved.
    distPrefElements = splitDirectories distPref
    distPrefBuild = case drop (length distPrefElements - 3) distPrefElements of
      ["t", _, "noopt"] ->
        joinPath $
          take (length distPrefElements - 3) distPrefElements
            ++ ["noopt"]
      ["t", _, "opt"] ->
        joinPath $
          take (length distPrefElements - 3) distPrefElements
            ++ ["opt"]
      [_, "t", _] ->
        joinPath $ take (length distPrefElements - 2) distPrefElements
      _ -> distPref

tixDir
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Component name
  -> FilePath
  -- ^ Directory containing test suite's .tix files
tixDir distPref way name = hpcDir distPref way </> "tix" </> name

-- | Path to the .tix file containing a test suite's sum statistics.
tixFilePath
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Component name
  -> FilePath
  -- ^ Path to test suite's .tix file
tixFilePath distPref way name = tixDir distPref way name </> name <.> "tix"

htmlDir
  :: FilePath
  -- ^ \"dist/\" prefix
  -> Way
  -> FilePath
  -- ^ Component name
  -> FilePath
  -- ^ Path to test suite's HTML markup directory
htmlDir distPref way name = hpcDir distPref way </> "html" </> name

-- | Attempt to guess the way the test suites in this package were compiled
-- and linked with the library so the correct module interfaces are found.
guessWay :: LocalBuildInfo -> Way
guessWay lbi
  | withProfExe lbi = Prof
  | withDynExe lbi = Dyn
  | otherwise = Vanilla

-- | Generate the HTML markup for a test suite.
markupTest
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ \"dist/\" prefix
  -> String
  -- ^ Library name
  -> TestSuite
  -> Library
  -> IO ()
markupTest verbosity lbi distPref libraryName suite library = do
  tixFileExists <- doesFileExist $ tixFilePath distPref way $ testName'
  when tixFileExists $ do
    -- behaviour of 'markup' depends on version, so we need *a* version
    -- but no particular one
    (hpc, hpcVer, _) <-
      requireProgramVersion
        verbosity
        hpcProgram
        anyVersion
        (withPrograms lbi)
    let htmlDir_ = htmlDir distPref way testName'
    markup
      hpc
      hpcVer
      verbosity
      (tixFilePath distPref way testName')
      mixDirs
      htmlDir_
      (exposedModules library)
    notice verbosity $
      "Test coverage report written to "
        ++ htmlDir_
        </> "hpc_index" <.> "html"
  where
    way = guessWay lbi
    testName' = unUnqualComponentName $ testName suite
    mixDirs = map (mixDir distPref way) [testName', libraryName]

-- | Generate the HTML markup for all of a package's test suites.
markupPackage
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ \"dist/\" prefix
  -> PD.PackageDescription
  -> [TestSuite]
  -> IO ()
markupPackage verbosity lbi distPref pkg_descr suites = do
  let tixFiles = map (tixFilePath distPref way) testNames
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
    let outFile = tixFilePath distPref way libraryName
        htmlDir' = htmlDir distPref way libraryName
        excluded = concatMap testModules suites ++ [main]
    createDirectoryIfMissing True $ takeDirectory outFile
    union hpc verbosity tixFiles outFile excluded
    markup hpc hpcVer verbosity outFile mixDirs htmlDir' included
    notice verbosity $
      "Package coverage report written to "
        ++ htmlDir'
        </> "hpc_index.html"
  where
    way = guessWay lbi
    testNames = fmap (unUnqualComponentName . testName) suites
    mixDirs = map (mixDir distPref way) $ libraryName : testNames
    included = concatMap (exposedModules) $ PD.allLibraries pkg_descr
    libraryName = prettyShow $ PD.package pkg_descr

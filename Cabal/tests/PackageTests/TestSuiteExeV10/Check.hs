module PackageTests.TestSuiteExeV10.Check
       ( checkTest
       , checkTestWithHpc
       ) where

import Distribution.PackageDescription ( TestSuite(..), emptyTestSuite )
import Distribution.Simple.Hpc
import Distribution.Version
import Test.HUnit
import System.Directory
import System.FilePath
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "TestSuiteExeV10"

checkTest :: Version -> Test
checkTest cabalVersion = TestCase $ do
    let spec = PackageSpec dir ["--enable-tests"]
    buildResult <- cabal_build spec
    assertBuildSucceeded buildResult
    testResult <- cabal_test spec []
    assertTestSucceeded testResult

checkTestWithHpc :: Version -> Test
checkTestWithHpc cabalVersion = TestCase $ do
    let spec = PackageSpec dir [ "--enable-tests"
                               , "--enable-library-coverage"
                               ]
    buildResult <- cabal_build spec
    assertBuildSucceeded buildResult
    testResult <- cabal_test spec []
    assertTestSucceeded testResult
    let dummy = emptyTestSuite { testName = "test-Foo" }
        tixFile = tixFilePath (dir </> "dist") $ testName dummy
        tixFileMessage = ".tix file should exist"
        markupDir = htmlDir (dir </> "dist") $ testName dummy
        markupFile = markupDir </> "hpc_index" <.> "html"
        markupFileMessage = "HPC markup file should exist"
    tixFileExists <- doesFileExist tixFile
    assertEqual tixFileMessage True tixFileExists
    markupFileExists <- doesFileExist markupFile
    assertEqual markupFileMessage True markupFileExists

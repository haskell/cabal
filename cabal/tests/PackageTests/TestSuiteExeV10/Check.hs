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
    let buildMessage = "\'setup build\' should succeed"
    assertEqual buildMessage True $ successful buildResult
    testResult <- cabal_test spec []
    let testMessage = "\'setup test\' should succeed"
    assertEqual testMessage True $ successful testResult

checkTestWithHpc :: Version -> Test
checkTestWithHpc cabalVersion = TestCase $ do
    let spec = PackageSpec dir [ "--enable-tests"
                               , "--enable-library-coverage"
                               ]
    buildResult <- cabal_build spec
    let buildMessage = "\'setup build\' should succeed"
    assertEqual buildMessage True $ successful buildResult
    testResult <- cabal_test spec []
    let testMessage = "\'setup test\' should succeed"
    assertEqual testMessage True $ successful testResult
    let dummy = emptyTestSuite { testName = "test-Foo" }
        tixFile = tixFilePath (dir </> "dist") dummy
        tixFileMessage = ".tix file should exist"
        markupDir = tixDir (dir </> "dist") dummy
        markupFile = markupDir </> "hpc_index" <.> "html"
        markupFileMessage = "HPC markup file should exist"
    tixFileExists <- doesFileExist tixFile
    assertEqual tixFileMessage True tixFileExists
    markupFileExists <- doesFileExist markupFile
    assertEqual markupFileMessage True markupFileExists

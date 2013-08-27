module PackageTests.TestSuiteExeV10.Check
       ( checkTest
       , checkTestWithHpc
       ) where

import Distribution.PackageDescription     ( TestSuite(..), emptyTestSuite )
import Distribution.Version                ( Version(..), orLaterVersion )
import Distribution.Simple.Hpc
import Distribution.Simple.Program.Builtin ( hpcProgram )
import Distribution.Simple.Program.Db      ( emptyProgramDb, configureProgram,
                                             requireProgramVersion )
import PackageTests.PackageTester
import qualified Control.Exception as E    ( IOException, catch )
import Control.Monad                       ( when )
import System.Directory
import System.FilePath
import Test.HUnit

import qualified Distribution.Verbosity as Verbosity

dir :: FilePath
dir = "PackageTests" </> "TestSuiteExeV10"

checkTest :: FilePath -> Test
checkTest ghcPath = TestCase $ do
    let spec = PackageSpec dir ["--enable-tests"]
    buildResult <- cabal_build spec ghcPath
    assertBuildSucceeded buildResult
    testResult <- cabal_test spec [] ghcPath
    assertTestSucceeded testResult

checkTestWithHpc :: FilePath -> Test
checkTestWithHpc ghcPath = TestCase $ do
    isCorrectVersion <- checkHpcVersion
    when isCorrectVersion $ do
      let spec = PackageSpec dir [ "--enable-tests"
                                 , "--enable-library-coverage"
                                 ]
      buildResult <- cabal_build spec ghcPath
      assertBuildSucceeded buildResult
      testResult <- cabal_test spec [] ghcPath
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
  where
    checkHpcVersion :: IO Bool
    checkHpcVersion = do
      let programDb' = emptyProgramDb
      let verbosity = Verbosity.normal
      let verRange  = orLaterVersion (Version [0,7] [])
      programDb <- configureProgram verbosity hpcProgram programDb'
      (requireProgramVersion verbosity hpcProgram verRange programDb
       >> return True) `catchIO` (\_ -> return False)

    -- Distirubution.Compat.Exception is hidden.
    catchIO :: IO a -> (E.IOException -> IO a) -> IO a
    catchIO = E.catch

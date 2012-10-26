module PackageTests.TestOptions.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "TestOptions")
               ["--enable-tests"]
    _ <- cabal_build spec
    result <- cabal_test spec ["--test-options=1 2 3"]
    let message = "\"cabal test\" did not pass the correct options to the "
                  ++ "test executable with \"--test-options\""
    assertEqual message True $ successful result
    result' <- cabal_test spec [ "--test-option=1"
                               , "--test-option=2"
                               , "--test-option=3"
                               ]
    let message' = "\"cabal test\" did not pass the correct options to the "
                   ++ "test executable with \"--test-option\""
    assertEqual message' True $ successful result'

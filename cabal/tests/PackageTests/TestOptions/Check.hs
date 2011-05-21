module PackageTests.TestOptions.Check where

import Test.HUnit
import System.FilePath
import PackageTests.PackageTester

suite :: Test
suite = TestCase $ do
    let directory = "PackageTests" </> "TestOptions"
        pdFile = directory </> "TestOptions" <.> "cabal"
        spec = PackageSpec directory ["--enable-tests"]
    _ <- cabal_build spec
    result <- cabal_test spec ["--test-options=1 2 3"]
    let message = "\"cabal test\" did not pass the correct options to the "
                  ++ "test executable with \"--test-options\""
    assertEqual message True $ successful result
    result' <- cabal_test spec [ "--test-option=1"
                               , "--test-option=2"
                               , "--test-option=3"
                               ]
    let message = "\"cabal test\" did not pass the correct options to the "
                  ++ "test executable with \"--test-option\""
    assertEqual message True $ successful result'

module PackageTests.TestOptions.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

suite :: SuiteConfig -> Assertion
suite config = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "TestOptions"
            , configOpts = ["--enable-tests"]
            , distPref = Nothing
            }
    _ <- cabal_build config spec
    result <- cabal_test config spec [] ["--test-options=1 2 3"]
    let message = "\"cabal test\" did not pass the correct options to the "
                  ++ "test executable with \"--test-options\""
    assertEqual message True $ successful result
    result' <- cabal_test config spec []
               [ "--test-option=1"
               , "--test-option=2"
               , "--test-option=3"
               ]
    let message' = "\"cabal test\" did not pass the correct options to the "
                   ++ "test executable with \"--test-option\""
    assertEqual message' True $ successful result'

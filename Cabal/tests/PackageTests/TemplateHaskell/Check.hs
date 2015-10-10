module PackageTests.TemplateHaskell.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

vanilla :: SuiteConfig -> Assertion
vanilla config = do
  let spec = PackageSpec
          { directory = "PackageTests" </> "TemplateHaskell" </> "vanilla"
          , configOpts = []
          , distPref = Nothing
          }
  result <- cabal_build config spec
  assertBuildSucceeded result

profiling :: SuiteConfig -> Assertion
profiling config = do
   let flags = ["--enable-library-profiling"
--                ,"--disable-library-vanilla"
               ,"--enable-profiling"]
       spec = PackageSpec
          { directory = "PackageTests" </> "TemplateHaskell" </> "profiling"
          , configOpts = flags
          , distPref = Nothing
          }
   result <- cabal_build config spec
   assertBuildSucceeded result

dynamic :: SuiteConfig -> Assertion
dynamic config = do
    let flags = ["--enable-shared"
--                ,"--disable-library-vanilla"
                ,"--enable-executable-dynamic"]
        spec = PackageSpec
            { directory = "PackageTests" </> "TemplateHaskell" </> "dynamic"
            , configOpts = flags
            , distPref = Nothing
            }
    result <- cabal_build config spec
    assertBuildSucceeded result

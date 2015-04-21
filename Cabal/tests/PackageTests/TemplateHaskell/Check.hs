module PackageTests.TemplateHaskell.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

vanilla :: IO TestsConfig -> Assertion
vanilla cfg = do
  let spec = PackageSpec
          { directory = "PackageTests" </> "TemplateHaskell" </> "vanilla"
          , configOpts = []
          , distPref = Nothing
          }
  result <- cabal_build cfg spec
  assertBuildSucceeded result

profiling :: IO TestsConfig -> Assertion
profiling cfg = do
   let flags = ["--enable-library-profiling"
--                ,"--disable-library-vanilla"
               ,"--enable-profiling"]
       spec = PackageSpec
          { directory = "PackageTests" </> "TemplateHaskell" </> "profiling"
          , configOpts = flags
          , distPref = Nothing
          }
   result <- cabal_build cfg spec
   assertBuildSucceeded result

dynamic :: IO TestsConfig -> Assertion
dynamic cfg = do
    let flags = ["--enable-shared"
--                ,"--disable-library-vanilla"
                ,"--enable-executable-dynamic"]
        spec = PackageSpec
            { directory = "PackageTests" </> "TemplateHaskell" </> "dynamic"
            , configOpts = flags
            , distPref = Nothing
            }
    result <- cabal_build cfg spec
    assertBuildSucceeded result

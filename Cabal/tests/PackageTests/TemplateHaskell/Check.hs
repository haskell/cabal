module PackageTests.TemplateHaskell.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

vanilla :: FilePath -> Assertion
vanilla ghcPath = do
  let spec = PackageSpec
          { directory = "PackageTests" </> "TemplateHaskell" </> "vanilla"
          , configOpts = []
          , distPref = Nothing
          }
  result <- cabal_build spec ghcPath
  assertBuildSucceeded result

profiling :: FilePath -> Assertion
profiling ghcPath = do
   let flags = ["--enable-library-profiling"
--                ,"--disable-library-vanilla"
               ,"--enable-profiling"]
       spec = PackageSpec
          { directory = "PackageTests" </> "TemplateHaskell" </> "profiling"
          , configOpts = flags
          , distPref = Nothing
          }
   result <- cabal_build spec ghcPath
   assertBuildSucceeded result

dynamic :: FilePath -> Assertion
dynamic ghcPath = do
    let flags = ["--enable-shared"
--                ,"--disable-library-vanilla"
                ,"--enable-executable-dynamic"]
        spec = PackageSpec
            { directory = "PackageTests" </> "TemplateHaskell" </> "dynamic"
            , configOpts = flags
            , distPref = Nothing
            }
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result

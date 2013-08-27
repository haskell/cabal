module PackageTests.TemplateHaskell.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

vanilla :: FilePath -> Test
vanilla ghcPath = TestCase $ do
  let spec = PackageSpec ("PackageTests" </>
                          "TemplateHaskell" </> "vanilla") []
  result <- cabal_build spec ghcPath
  assertBuildSucceeded result

profiling :: FilePath -> Test
profiling ghcPath = TestCase $ do
   let flags = ["--enable-library-profiling"
--                ,"--disable-library-vanilla"
               ,"--enable-executable-profiling"]
       spec = PackageSpec ("PackageTests" </>
                           "TemplateHaskell" </> "profiling") flags
   result <- cabal_build spec ghcPath
   assertBuildSucceeded result

dynamic :: FilePath -> Test
dynamic ghcPath = TestCase $ do
    let flags = ["--enable-shared"
--                ,"--disable-library-vanilla"
                ,"--enable-executable-dynamic"]
        spec = PackageSpec ("PackageTests" </>
                            "TemplateHaskell" </> "dynamic") flags
    result <- cabal_build spec ghcPath
    assertBuildSucceeded result

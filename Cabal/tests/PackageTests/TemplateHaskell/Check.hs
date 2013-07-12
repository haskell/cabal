module PackageTests.TemplateHaskell.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

vanilla :: Test
vanilla = TestCase $ do
  let spec = PackageSpec ("PackageTests" </>
                          "TemplateHaskell" </> "vanilla") []
  result <- cabal_build spec
  assertBuildSucceeded result

profiling :: Test
profiling = TestCase $ do
   let flags = ["--enable-library-profiling"
--                ,"--disable-library-vanilla"
               ,"--enable-executable-profiling"]
       spec = PackageSpec ("PackageTests" </>
                           "TemplateHaskell" </> "profiling") flags
   result <- cabal_build spec
   assertBuildSucceeded result

dynamic :: Test
dynamic = TestCase $ do
    let flags = ["--enable-shared"
--                ,"--disable-library-vanilla"
                ,"--enable-executable-dynamic"]
        spec = PackageSpec ("PackageTests" </>
                            "TemplateHaskell" </> "dynamic") flags
    result <- cabal_build spec
    assertBuildSucceeded result

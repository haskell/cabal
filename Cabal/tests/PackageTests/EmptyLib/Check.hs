module PackageTests.EmptyLib.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

-- See https://github.com/haskell/cabal/issues/1241
emptyLib :: Test
emptyLib = TestCase $ do
   let spec = PackageSpec ("PackageTests" </> "EmptyLib"
                           </> "empty") []
   result <- cabal_build spec
   assertBuildSucceeded result

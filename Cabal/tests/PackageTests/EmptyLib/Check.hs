module PackageTests.EmptyLib.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit

-- See https://github.com/haskell/cabal/issues/1241
emptyLib :: SuiteConfig -> Assertion
emptyLib config = do
   let spec = PackageSpec
          { directory = "PackageTests" </> "EmptyLib" </> "empty"
          , configOpts = []
          , distPref = Nothing
          }
   result <- cabal_build config spec
   assertBuildSucceeded result

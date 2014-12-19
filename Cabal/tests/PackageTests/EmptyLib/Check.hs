module PackageTests.EmptyLib.Check where

import PackageTests.PackageTester
import System.FilePath
import Test.HUnit

-- See https://github.com/haskell/cabal/issues/1241
emptyLib :: FilePath -> Test
emptyLib ghcPath = TestCase $ do
   let spec = PackageSpec
          { directory = "PackageTests" </> "EmptyLib" </> "empty"
          , configOpts = []
          , distPref = Nothing
          }
   result <- cabal_build spec ghcPath
   assertBuildSucceeded result

module PackageTests.CMain.Check
       ( checkBuild
       ) where

import Test.HUnit
import System.FilePath
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "CMain"

checkBuild :: Test
checkBuild = TestCase $ do
    let spec = PackageSpec dir []
    buildResult <- cabal_build spec
    assertBuildSucceeded buildResult

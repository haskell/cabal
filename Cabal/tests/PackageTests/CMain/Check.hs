module PackageTests.CMain.Check
       ( checkBuild
       ) where

import Distribution.Simple.Hpc
import Distribution.Version
import Test.HUnit
import System.Directory
import System.FilePath
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "CMain"

checkBuild :: Test
checkBuild = TestCase $ do
    let spec = PackageSpec dir []
    buildResult <- cabal_build spec
    assertBuildSucceeded buildResult

module PackageTests.CMain.Check
       ( checkBuild
       ) where

import Test.Tasty.HUnit
import System.FilePath
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "CMain"

checkBuild :: SuiteConfig -> Assertion
checkBuild config = do
    let spec = PackageSpec
            { directory = dir
            , distPref = Nothing
            , configOpts = []
            }
    buildResult <- cabal_build config spec
    assertBuildSucceeded buildResult

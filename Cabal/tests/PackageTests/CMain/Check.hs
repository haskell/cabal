module PackageTests.CMain.Check
       ( checkBuild
       ) where

import Test.Tasty.HUnit
import System.FilePath
import PackageTests.PackageTester

dir :: FilePath
dir = "PackageTests" </> "CMain"

checkBuild :: IO TestsConfig -> Assertion
checkBuild cfg = do
    let spec = PackageSpec
            { directory = dir
            , distPref = Nothing
            , configOpts = []
            }
    buildResult <- cabal_build cfg spec
    assertBuildSucceeded buildResult

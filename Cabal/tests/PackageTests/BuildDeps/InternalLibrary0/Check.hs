module PackageTests.BuildDeps.InternalLibrary0.Check where

import Control.Monad
import Data.Version
import PackageTests.PackageTester
import System.FilePath
import Test.Tasty.HUnit


suite :: SuiteConfig -> Version  -> Assertion
suite config cabalVersion = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "InternalLibrary0"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build config spec
    assertBuildFailed result
    when (cabalVersion >= Version [1, 7] []) $ do
        let sb = "library which is defined within the same package."
        -- In 1.7 it should tell you how to enable the desired behaviour.
        assertOutputContains sb result

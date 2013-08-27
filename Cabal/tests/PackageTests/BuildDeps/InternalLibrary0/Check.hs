module PackageTests.BuildDeps.InternalLibrary0.Check where

import Control.Monad
import Data.Version
import PackageTests.PackageTester
import System.FilePath
import Test.HUnit


suite :: Version -> FilePath -> Test
suite cabalVersion ghcPath = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary0") []
    result <- cabal_build spec ghcPath
    assertBuildFailed result
    when (cabalVersion >= Version [1, 7] []) $ do
        let sb = "library which is defined within the same package."
        -- In 1.7 it should tell you how to enable the desired behaviour.
        assertOutputContains sb result

module PackageTests.BuildDeps.InternalLibrary0.Check where

import Test.HUnit
import PackageTests.PackageTester
import Control.Monad
import System.FilePath
import Data.Version
import Data.List (isInfixOf, intercalate)
import Control.Exception
import Prelude hiding (catch)


suite :: Version -> Test
suite cabalVersion = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary0") []
    result <- cabal_build spec
    assertBuildFailed result
    when (cabalVersion >= Version [1, 7] []) $ do
        let sb = "library which is defined within the same package."
        -- In 1.7 it should tell you how to enable the desired behaviour.
        assertOutputContains sb result

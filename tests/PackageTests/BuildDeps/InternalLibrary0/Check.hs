module PackageTests.BuildDeps.InternalLibrary0.Check where

import Test.HUnit
import PackageTests.PackageTester
import Control.Monad
import System.FilePath
import Data.Version
import Data.List (isInfixOf, intercalate)


suite :: Version -> Test
suite cabalVersion = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary0") []
    result <- cabal_build spec
    assertEqual "cabal build should fail" False (successful result)
    when (cabalVersion >= Version [1, 7] []) $ do
        -- In 1.7 it should tell you how to enable the desired behaviour.
        assertEqual "error should say 'library which is defined within the same package.'" True $
            "library which is defined within the same package." `isInfixOf` (intercalate " " $ lines $ outputText result)


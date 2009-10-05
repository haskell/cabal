module PackageTests.BuildDeps.InternalLibrary4.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import Data.List


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "InternalLibrary4") []
    let specTI = PackageSpec (directory spec </> "to-install") []

    unregister "InternalLibrary4"
    iResult <- cabal_install specTI                     
    assertEqual "cabal install should succeed - see to-install/test-log.txt" True (successful iResult)
    bResult <- cabal_build spec
    assertEqual "cabal build should fail - see test-log.txt" False (successful bResult)
    unregister "InternalLibrary4"

    assertEqual "error should say 'cannot satisfy dependency InternalLibrary4 >=0.2'" True $
        "cannot satisfy dependency InternalLibrary4 >=0.2" `isInfixOf` (intercalate " " $ lines $ outputText bResult)


module PackageTests.BuildDeps.TargetSpecificDeps1.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import Data.List
import qualified Control.Exception as E


suite :: Test
suite = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "TargetSpecificDeps1") []
    result <- cabal_build spec
    do
        assertEqual "cabal build should fail - see test-log.txt" False (successful result)
        assertBool "error should be in MyLibrary.hs" $
            "MyLibrary.hs:" `isInfixOf` outputText result
        assertBool "error should be \"Could not find module `System.Time\"" $
            "Could not find module `System.Time'" `isInfixOf`
                            (intercalate " " $ lines $ outputText result)
      `E.catch` \exc -> do
        putStrLn $ "Cabal result was "++show result
        E.throwIO (exc :: E.SomeException)

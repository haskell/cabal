module PackageTests.BuildDeps.TargetSpecificDeps1.Check where

import Test.Tasty.HUnit
import PackageTests.PackageTester
import System.FilePath
import Data.List
import qualified Control.Exception as E
import Text.Regex.Posix


suite :: SuiteConfig -> Assertion
suite config = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "TargetSpecificDeps1"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build config spec
    do
        assertEqual "cabal build should fail - see test-log.txt" False (successful result)
        assertBool "error should be in MyLibrary.hs" $
            "MyLibrary.hs:" `isInfixOf` outputText result
        assertBool "error should be \"Could not find module `System.Time\"" $
            (intercalate " " $ lines $ outputText result)
            =~ "Could not find module.*System.Time"

      `E.catch` \exc -> do
        putStrLn $ "Cabal result was "++show result
        E.throwIO (exc :: E.SomeException)

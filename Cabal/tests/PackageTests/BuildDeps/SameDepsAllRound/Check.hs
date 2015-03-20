module PackageTests.BuildDeps.SameDepsAllRound.Check where

import Test.Tasty.HUnit
import PackageTests.PackageTester
import System.FilePath
import qualified Control.Exception as E


suite :: FilePath -> Assertion
suite ghcPath = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "BuildDeps" </> "SameDepsAllRound"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build spec ghcPath
    do
        assertEqual "cabal build should succeed - see test-log.txt" True (successful result)
      `E.catch` \exc -> do
        putStrLn $ "Cabal result was "++show result
        E.throwIO (exc :: E.SomeException)

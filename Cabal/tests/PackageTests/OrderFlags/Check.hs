module PackageTests.OrderFlags.Check where

import Test.HUnit
import PackageTests.PackageTester
import System.FilePath
import Control.Exception
import Prelude hiding (catch)


suite :: FilePath -> Test
suite ghcPath = TestCase $ do
    let spec = PackageSpec ("PackageTests" </> "OrderFlags") []
    result <- cabal_build spec ghcPath
    do
        assertEqual "cabal build should succeed - see test-log.txt" True (successful result)
      `catch` \exc -> do
        putStrLn $ "Cabal result was "++show result
        throwIO (exc :: SomeException)

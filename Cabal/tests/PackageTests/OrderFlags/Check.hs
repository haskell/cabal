module PackageTests.OrderFlags.Check where

import Test.Tasty.HUnit
import PackageTests.PackageTester
import System.FilePath
import Control.Exception

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

suite :: IO TestsConfig -> Assertion
suite cfg = do
    let spec = PackageSpec
            { directory = "PackageTests" </> "OrderFlags"
            , configOpts = []
            , distPref = Nothing
            }
    result <- cabal_build cfg spec
    do
        assertEqual "cabal build should succeed - see test-log.txt" True (successful result)
      `catch` \exc -> do
        putStrLn $ "Cabal result was "++show result
        throwIO (exc :: SomeException)

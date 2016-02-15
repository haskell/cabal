module UnitTests.Distribution.Client.Compat.Time (tests) where

import Control.Concurrent (threadDelay)
import System.FilePath

import Distribution.Simple.Utils (withTempDirectory)
import Distribution.Verbosity

import Distribution.Client.Compat.Time

import Test.Tasty
import Test.Tasty.HUnit

-- TODO: Calibrate, like Shake's test suite does.
mtimeDelay :: Int
mtimeDelay = 500000 -- 0.5 s

tests :: [TestTree]
tests =
  [ testCase "getModTime has sub-second resolution" getModTimeTest
  , testCase "getCurTime works as expected"         getCurTimeTest ]

getModTimeTest :: Assertion
getModTimeTest =
  withTempDirectory silent "." "getmodtime-" $ \dir -> do
    let fileName = dir </> "foo"
    writeFile fileName "bar"
    t0 <- getModTime fileName
    threadDelay mtimeDelay
    writeFile fileName "baz"
    t1 <- getModTime fileName
    assertBool "expected different file mtimes" (t1 > t0)


getCurTimeTest :: Assertion
getCurTimeTest =
  withTempDirectory silent "." "getmodtime-" $ \dir -> do
    let fileName = dir </> "foo"
    writeFile fileName "bar"
    t0 <- getModTime fileName
    threadDelay mtimeDelay
    t1 <- getCurTime
    assertBool("expected file mtime (" ++ show t0
               ++ ") to be earlier than current time (" ++ show t1 ++ ")")
      (t0 < t1)

    threadDelay mtimeDelay
    writeFile fileName "baz"
    t2 <- getModTime fileName
    assertBool ("expected current time (" ++ show t1
                ++ ") to be earlier than file mtime (" ++ show t2 ++ ")")
      (t1 < t2)

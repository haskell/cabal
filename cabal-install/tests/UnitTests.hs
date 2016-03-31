{-# LANGUAGE ScopedTypeVariables #-}

module Main
       where

import Test.Tasty

import Control.Monad
import Data.Time.Clock
import System.FilePath

import Distribution.Simple.Utils
import Distribution.Verbosity

import Distribution.Client.Compat.Time

import qualified UnitTests.Distribution.Client.Compat.Time
import qualified UnitTests.Distribution.Client.Dependency.Modular.PSQ
import qualified UnitTests.Distribution.Client.Dependency.Modular.Solver
import qualified UnitTests.Distribution.Client.FileMonitor
import qualified UnitTests.Distribution.Client.Glob
import qualified UnitTests.Distribution.Client.GZipUtils
import qualified UnitTests.Distribution.Client.Sandbox
import qualified UnitTests.Distribution.Client.Sandbox.Timestamp
import qualified UnitTests.Distribution.Client.Tar
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.UserConfig
import qualified UnitTests.Distribution.Client.ProjectConfig

import UnitTests.Options


tests :: Int -> TestTree
tests mtimeChangeCalibrated =
  askOption $ \(OptionMtimeChangeDelay mtimeChangeProvided) ->
  let mtimeChange = if mtimeChangeProvided /= 0
                    then mtimeChangeProvided
                    else mtimeChangeCalibrated
  in
  testGroup "Unit Tests"
  [ testGroup "UnitTests.Distribution.Client.Compat.Time" $
        UnitTests.Distribution.Client.Compat.Time.tests mtimeChange
  , testGroup "UnitTests.Distribution.Client.Dependency.Modular.PSQ"
        UnitTests.Distribution.Client.Dependency.Modular.PSQ.tests
  , testGroup "UnitTests.Distribution.Client.Dependency.Modular.Solver"
        UnitTests.Distribution.Client.Dependency.Modular.Solver.tests
  , testGroup "UnitTests.Distribution.Client.FileMonitor" $
        UnitTests.Distribution.Client.FileMonitor.tests mtimeChange
  , testGroup "UnitTests.Distribution.Client.Glob"
        UnitTests.Distribution.Client.Glob.tests
  , testGroup "Distribution.Client.GZipUtils"
       UnitTests.Distribution.Client.GZipUtils.tests
  , testGroup "Distribution.Client.Sandbox"
       UnitTests.Distribution.Client.Sandbox.tests
  , testGroup "Distribution.Client.Sandbox.Timestamp"
       UnitTests.Distribution.Client.Sandbox.Timestamp.tests
  , testGroup "Distribution.Client.Tar"
       UnitTests.Distribution.Client.Tar.tests
  , testGroup "Distribution.Client.Targets"
       UnitTests.Distribution.Client.Targets.tests
  , testGroup "UnitTests.Distribution.Client.UserConfig"
       UnitTests.Distribution.Client.UserConfig.tests
  , testGroup "UnitTests.Distribution.Client.ProjectConfig"
       UnitTests.Distribution.Client.ProjectConfig.tests
  ]

main :: IO ()
main = do
  mtimeChangeDelay <- calibrateMtimeChangeDelay
  defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         (tests mtimeChangeDelay)

-- Based on code written by Neill Mitchell for Shake. See
-- 'sleepFileTimeCalibrate' in 'Test.Type'. The returned delay is never smaller
-- than 10 ms, but never larger than 1 second.
calibrateMtimeChangeDelay :: IO Int
calibrateMtimeChangeDelay = do
  withTempDirectory silent "." "calibration-" $ \dir -> do
    let fileName = dir </> "probe"
    mtimes <- forM [1..25] $ \(i::Int) -> time $ do
      writeFile fileName $ show i
      t0 <- getModTime fileName
      let spin j = do
            writeFile fileName $ show (i,j)
            t1 <- getModTime fileName
            unless (t0 < t1) (spin $ j + 1)
      spin (0::Int)
    let mtimeChange  = maximum mtimes
        mtimeChange' = min 1000000 $ (max 10000 mtimeChange) * 2
    notice normal $ "File modification time resolution calibration completed, "
      ++ "maximum delay observed: "
      ++ (show . toMillis $ mtimeChange ) ++ " ms. "
      ++ "Will be using delay of " ++ (show . toMillis $ mtimeChange')
      ++ " for test runs."
    return mtimeChange'
  where
    toMillis :: Int -> Double
    toMillis x = fromIntegral x / 1000.0

    time :: IO () -> IO Int
    time act = do
      t0 <- getCurrentTime
      act
      t1 <- getCurrentTime
      return . ceiling $! (t1 `diffUTCTime` t0) * 1e6 -- microseconds

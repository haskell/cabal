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
import qualified UnitTests.Distribution.Client.GZipUtils
import qualified UnitTests.Distribution.Client.Sandbox
import qualified UnitTests.Distribution.Client.Sandbox.Timestamp
import qualified UnitTests.Distribution.Client.Tar
import qualified UnitTests.Distribution.Client.Targets
import qualified UnitTests.Distribution.Client.UserConfig

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
  ]

main :: IO ()
main = do
  mtimeChangeDelay <- calibrateMtimeChangeDelay
  defaultMainWithIngredients
         (includingOptions extraOptions : defaultIngredients)
         (tests mtimeChangeDelay)

-- Based on code written by Neill Mitchell for Shake. See
-- 'sleepFileTimeCalibrate' in 'Test.Type'.
calibrateMtimeChangeDelay :: IO Int
calibrateMtimeChangeDelay = do
  withTempDirectory silent "." "calibration-" $ \dir -> do
    let fileName = dir </> "probe"
    mtimes <- forM [1..10] $ \(i::Int) -> time $ do
      writeFile fileName $ show i
      t0 <- getModTime fileName
      let spin j = do
            writeFile fileName $ show (i,j)
            t1 <- getModTime fileName
            unless (t0 < t1) (spin $ j + 1)
      spin (0::Int)
    let mtimeChange = maximum mtimes
    putStrLn $ "Mtime delay calibration completed, calculated delay: "
      ++ (show mtimeChange) ++ " ms."
    return $ min 1000000 mtimeChange
  where
    time :: IO () -> IO Int
    time act = do
      t0 <- getCurrentTime
      act
      t1 <- getCurrentTime
      return . ceiling $! (t1 `diffUTCTime` t0) * 1e6 -- microseconds

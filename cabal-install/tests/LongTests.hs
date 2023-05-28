module Main (main) where

import Test.Tasty

import Distribution.Compat.Time
import Distribution.Simple.Utils
import Distribution.Verbosity

import qualified UnitTests.Distribution.Client.Described
import qualified UnitTests.Distribution.Client.FileMonitor
import qualified UnitTests.Distribution.Client.VCS
import qualified UnitTests.Distribution.Solver.Modular.QuickCheck
import UnitTests.Options

main :: IO ()
main = do
  (mtimeChange, mtimeChange') <- calibrateMtimeChangeDelay
  let toMillis :: Int -> Double
      toMillis x = fromIntegral x / 1000.0
  notice normal $
    "File modification time resolution calibration completed, "
      ++ "maximum delay observed: "
      ++ (show . toMillis $ mtimeChange)
      ++ " ms. "
      ++ "Will be using delay of "
      ++ (show . toMillis $ mtimeChange')
      ++ " for test runs."
  defaultMainWithIngredients
    (includingOptions extraOptions : defaultIngredients)
    (tests mtimeChange')

tests :: Int -> TestTree
tests mtimeChangeCalibrated =
  askOption $ \(OptionMtimeChangeDelay mtimeChangeProvided) ->
    let mtimeChange =
          if mtimeChangeProvided /= 0
            then mtimeChangeProvided
            else mtimeChangeCalibrated
     in testGroup
          "Long-running tests"
          [ testGroup
              "Solver QuickCheck"
              UnitTests.Distribution.Solver.Modular.QuickCheck.tests
          , testGroup "UnitTests.Distribution.Client.VCS" $
              UnitTests.Distribution.Client.VCS.tests mtimeChange
          , testGroup "UnitTests.Distribution.Client.FileMonitor" $
              UnitTests.Distribution.Client.FileMonitor.tests mtimeChange
          , UnitTests.Distribution.Client.Described.tests
          ]

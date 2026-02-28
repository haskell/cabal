module Main (main) where

import Test.Tasty

import qualified UnitTests.Distribution.Client.Described
import qualified UnitTests.Distribution.Client.FileMonitor
import qualified UnitTests.Distribution.Client.VCS
import qualified UnitTests.Distribution.Solver.Modular.QuickCheck
import UnitTests.Options

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions extraOptions : defaultIngredients)
    tests

tests :: TestTree
tests =
  askOption $ \(OptionMtimeChangeDelay mtimeChange) ->
    testGroup
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

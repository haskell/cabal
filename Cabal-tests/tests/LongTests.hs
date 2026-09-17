module Main (main) where

import Test.Tasty

import qualified UnitTests.Distribution.Simple.FileMonitor
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
      [ testGroup "UnitTests.Distribution.Simple.FileMonitor" $
          UnitTests.Distribution.Simple.FileMonitor.tests mtimeChange
      ]

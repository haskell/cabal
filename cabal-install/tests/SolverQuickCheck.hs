module Main where

import Test.Tasty

import qualified UnitTests.Distribution.Client.Dependency.Modular.QuickCheck


tests :: TestTree
tests =
  testGroup "Solver QuickCheck"
  [ testGroup "UnitTests.Distribution.Client.Dependency.Modular.QuickCheck"
        UnitTests.Distribution.Client.Dependency.Modular.QuickCheck.tests
  ]

main :: IO ()
main = defaultMain tests

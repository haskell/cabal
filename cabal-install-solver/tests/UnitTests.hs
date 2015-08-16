module Main
       where

import Test.Tasty

import qualified UnitTests.Distribution.Client.Dependency.Modular.PSQ

tests :: TestTree
tests = testGroup "Unit Tests" [
   testGroup "UnitTests.Distribution.Client.Dependency.Modular.PSQ"
        UnitTests.Distribution.Client.Dependency.Modular.PSQ.tests
  ]

main :: IO ()
main = defaultMainWithIngredients defaultIngredients tests

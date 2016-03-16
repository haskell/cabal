{-# LANGUAGE ScopedTypeVariables #-}

module Main
       where

import Test.Tasty

import qualified UnitTests.Distribution.Solver.Modular.Internal.PSQ

tests :: TestTree
tests =
  testGroup "Unit Tests"
    [ testGroup "UnitTests.Distribution.Solver.Modular.Internal.PSQ"
          UnitTests.Distribution.Solver.Modular.Internal.PSQ.tests
    ]

main :: IO ()
main = do
  defaultMainWithIngredients
         defaultIngredients
         tests

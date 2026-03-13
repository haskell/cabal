module UnitTests.Distribution.Solver.Modular.Builder
  ( tests
  ) where

import Distribution.Solver.Modular.Builder

import Data.Bifunctor (second)
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testProperty "splitsAltImplementation" splitsTest
  ]

-- | Simpler splits implementation
splits' :: [a] -> [(a, [a])]
splits' [] = []
splits' (x : xs) = (x, xs) : map (second (x :)) (splits' xs)

splitsTest :: [Int] -> Property
splitsTest xs = splits' xs === splits xs

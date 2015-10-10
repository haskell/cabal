{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Poly where

import Data.Monoid (Monoid)

import Test.QuickCheck (Arbitrary, CoArbitrary)
import Text.Show.Functions

newtype A = A Int deriving (Eq, Show, Arbitrary, CoArbitrary)
newtype B = B Int deriving (Eq, Show, Arbitrary, CoArbitrary)
newtype C = C Int deriving (Eq, Show, Arbitrary, CoArbitrary)

newtype M = M [Int] deriving (Eq, Show, Arbitrary, CoArbitrary, Monoid)

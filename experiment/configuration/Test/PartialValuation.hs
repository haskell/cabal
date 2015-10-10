{-# LANGUAGE StandaloneDeriving #-}
module Test.PartialValuation where

import BasePredicate
import PartialValuation

import Test.QuickCheck
import Test.Poly (A)

import Data.Monoid
         ( Monoid(..) )


instance CoArbitrary a => Arbitrary (PartialValuation a) where
  arbitrary = fmap PartialValuation arbitrary

deriving instance Show (PartialValuation a)

infix 4 ===

(===) :: (Arbitrary a, Show a) => PartialValuation a -> PartialValuation a -> Property
v === v' = forAll arbitrary $ \e ->
                applyPartialValuation v  e
             == applyPartialValuation v' e

prop_monoid_1 :: PartialValuation A -> Property
prop_monoid_1 v = mempty `mappend` v === v

prop_monoid_2 :: PartialValuation A -> Property
prop_monoid_2 v = v `mappend` mempty === v

prop_monoid_3 :: PartialValuation A -> PartialValuation A -> PartialValuation A -> Property
prop_monoid_3 x y z = (x `mappend`  y) `mappend` z
                  ===  x `mappend` (y  `mappend` z)

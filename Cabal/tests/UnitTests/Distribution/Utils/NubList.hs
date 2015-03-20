{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Utils.NubList
    ( tests
    ) where

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Distribution.Utils.NubList
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ testCase "Numlist retains ordering" testOrdering
    , testCase "Numlist removes duplicates" testDeDupe
    , testProperty "Monoid Numlist Identity" prop_Identity
    , testProperty "Monoid Numlist Associativity" prop_Associativity
    ]

someIntList :: [Int]
-- This list must not have duplicate entries.
someIntList = [ 1, 3, 4, 2, 0, 7, 6, 5, 9, -1 ]

testOrdering :: Assertion
testOrdering =
    assertBool "Maintains element ordering:" $
        fromNubList (toNubList someIntList) == someIntList

testDeDupe :: Assertion
testDeDupe =
    assertBool "De-duplicates a list:" $
        fromNubList (toNubList (someIntList ++ someIntList)) == someIntList

-- ---------------------------------------------------------------------------
-- QuickCheck properties for NubList

prop_Identity :: [Int] -> Bool
prop_Identity xs =
    mempty `mappend` toNubList xs == toNubList xs `mappend` mempty

prop_Associativity :: [Int] -> [Int] -> [Int] -> Bool
prop_Associativity xs ys zs =
    (toNubList xs `mappend` toNubList ys) `mappend` toNubList zs
            == toNubList xs `mappend` (toNubList ys `mappend` toNubList zs)

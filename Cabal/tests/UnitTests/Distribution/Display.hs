{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Distribution.Display
    ( tests
    , displayRoundtrip
    ) where

import Distribution.Display (Display(..), display, simpleParsec)
import Test.Tasty
import Test.Tasty.QuickCheck

displayRoundtrip :: (Arbitrary a, Show a, Eq a, Display a) => a -> Property
displayRoundtrip x = simpleParsec (display x) === Just x

tests :: [TestTree]
tests =
    [ testProperty "Display Bool round trip"  (displayRoundtrip :: Bool -> Property)
    ]
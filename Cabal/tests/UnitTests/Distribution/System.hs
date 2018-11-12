module UnitTests.Distribution.System
    ( tests
    ) where

import Distribution.Arbitrary.Instances ()
import Distribution.Text (Text(..), display, simpleParse)
import Distribution.System
import Test.Tasty
import Test.Tasty.QuickCheck

textRoundtrip :: (Show a, Eq a, Text a) => a -> Property
textRoundtrip x = simpleParse (display x) === Just x

tests :: [TestTree]
tests =
    [ testProperty "Text OS round trip"       (textRoundtrip :: OS -> Property)
    , testProperty "Text Arch round trip"     (textRoundtrip :: Arch -> Property)
    , testProperty "Text Platform round trip" (textRoundtrip :: Platform -> Property)
    ]

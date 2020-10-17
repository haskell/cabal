{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Distribution.System
    ( tests
    ) where

import Distribution.Parsec
import Distribution.Pretty
import Distribution.System
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.Cabal ()

textRoundtrip :: (Show a, Eq a, Pretty a, Parsec a) => a -> Property
textRoundtrip x = simpleParsec (prettyShow x) === Just x

tests :: [TestTree]
tests =
    [ testProperty "Text OS round trip"       (textRoundtrip :: OS -> Property)
    , testProperty "Text Arch round trip"     (textRoundtrip :: Arch -> Property)
    , testProperty "Text Platform round trip" (textRoundtrip :: Platform -> Property)
    ]

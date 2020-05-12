{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module UnitTests.Distribution.CabalSpecVersion (tests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.CabalSpecVersion
import Distribution.FieldGrammar.Newtypes (SpecVersion (..))
import Distribution.Parsec                (eitherParsec)
import Distribution.Pretty                (prettyShow)

import Test.Tasty
import Test.Tasty.QuickCheck

-- instances
import Test.QuickCheck.Instances.Cabal ()

tests :: TestTree
tests = testGroup "Distribution.CabalSpecVersion"
    [ testProperty "roundtrip" propRoundtrip
    , testProperty "fromVersionDigits . toVersionDigits = Just" propViaVersionDigits
    ]
  where
    -- we test roundtrip here,
    -- because Described instance is a small simplification.
    propRoundtrip :: SpecVersion -> Property
    propRoundtrip x = counterexample (show (res, str)) $ case res of
        Right y -> x == y
        Left _  -> False
      where
        str = prettyShow x
        res = eitherParsec str

    propViaVersionDigits :: CabalSpecVersion -> Property
    propViaVersionDigits csv =
        counterexample (show digits) $
        lhs === rhs
      where
        digits = cabalSpecToVersionDigits csv
        lhs    = cabalSpecFromVersionDigits digits
        rhs    = Just csv

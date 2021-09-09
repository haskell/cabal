{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module UnitTests.Distribution.PkgconfigVersion (pkgconfigVersionTests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Distribution.Parsec                      (eitherParsec)
import Distribution.Pretty
import Distribution.Types.PkgconfigVersionRange

import Test.QuickCheck.Instances.Cabal ()

pkgconfigVersionTests :: [TestTree]
pkgconfigVersionTests =
    [ testProperty "simpleParsec . prettyShow = Just" prop_parse_disp
    ]

prop_parse_disp :: PkgconfigVersionRange -> Property
prop_parse_disp vr = counterexample (show (prettyShow vr)) $
    eitherParsec (prettyShow vr) === Right vr

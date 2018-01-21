{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
module UnitTests.Distribution.SPDX (spdxTests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.SPDX
import Distribution.Parsec.Class (eitherParsec)
import Distribution.Pretty (prettyShow)

import Test.Tasty
import Test.Tasty.QuickCheck

spdxTests :: [TestTree]
spdxTests =
    [ testProperty "LicenseId roundtrip" licenseIdRoundtrip
    , testProperty "LicenseExceptionId roundtrip" licenseExceptionIdRoundtrip
    , testProperty "LicenseRef roundtrip" licenseRefRoundtrip
    , testProperty "SimpleLicenseExpression roundtrip" simpleLicenseExpressionRoundtrip
    , testProperty "LicenseExpression roundtrip" licenseExpressionRoundtrip
    ]

licenseIdRoundtrip :: LicenseId -> Property
licenseIdRoundtrip x = 
    counterexample (prettyShow x) $
    Right x === eitherParsec (prettyShow x)

licenseExceptionIdRoundtrip :: LicenseExceptionId -> Property
licenseExceptionIdRoundtrip x =
    counterexample (prettyShow x) $
    Right x === eitherParsec (prettyShow x)

licenseRefRoundtrip :: LicenseRef -> Property
licenseRefRoundtrip x =
    counterexample (prettyShow x) $
    Right x === eitherParsec (prettyShow x)

simpleLicenseExpressionRoundtrip :: SimpleLicenseExpression -> Property
simpleLicenseExpressionRoundtrip x = 
    counterexample (prettyShow x) $
    Right x === eitherParsec (prettyShow x)

licenseExpressionRoundtrip :: LicenseExpression -> Property
licenseExpressionRoundtrip x = 
    counterexample (prettyShow x) $
    Right (reassoc x) === eitherParsec (prettyShow x)

-- Parser produces right biased trees of and/or expressions
reassoc :: LicenseExpression -> LicenseExpression
reassoc (EOr a b) = case reassoc a of
    EOr x y -> EOr x (reassoc (EOr y b))
    x       -> EOr x (reassoc b)
reassoc (EAnd a b) = case reassoc a of
    EAnd x y -> EAnd x (reassoc (EAnd y b))
    x       -> EAnd x (reassoc b)
reassoc l = l

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Arbitrary LicenseId where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary LicenseExceptionId where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary LicenseRef where
    arbitrary = mkLicenseRef' <$> ids' <*> ids
      where
        ids = listOf1 $ elements $ ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0'..'9'] ++ "_-"
        ids' = oneof [ pure Nothing, Just <$> ids ]

instance Arbitrary SimpleLicenseExpression where
    arbitrary = oneof
        [ ELicenseId <$> arbitrary
        , ELicenseIdPlus <$> arbitrary
        , ELicenseRef <$> arbitrary
        ]

instance Arbitrary LicenseExpression where
    arbitrary = sized arb
      where
        arb n
            | n <= 0     = ELicense <$> arbitrary <*> pure Nothing
            | otherwise = oneof
                [ ELicense <$> arbitrary <*> arbitrary
                , EAnd <$> arbA <*> arbB
                , EOr <$> arbA <*> arbB
                ]
              where
                m = n `div` 2
                arbA = arb m 
                arbB = arb (n - m)

    shrink (EAnd a b) = a : b : map (uncurry EAnd) (shrink (a, b))
    shrink (EOr a b)  = a : b : map (uncurry EOr) (shrink (a, b))
    shrink _          = []


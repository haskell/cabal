{-# OPTIONS_GHC -fno-warn-deprecations #-}
module UnitTests.Distribution.SPDX (spdxTests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Distribution.SPDX
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.QuickCheck.Instances.Cabal ()

spdxTests :: [TestTree]
spdxTests =
    [ testProperty "LicenseId roundtrip" licenseIdRoundtrip
    , testProperty "LicenseExceptionId roundtrip" licenseExceptionIdRoundtrip
    , testProperty "LicenseRef roundtrip" licenseRefRoundtrip
    , testProperty "SimpleLicenseExpression roundtrip" simpleLicenseExpressionRoundtrip
    , testProperty "LicenseExpression roundtrip" licenseExpressionRoundtrip
    , testProperty "isAcceptableLicense l = True"  shouldAcceptProp
    , testProperty "isAcceptableLicense l = False" shouldRejectProp
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
-- isAcceptableLicence
-------------------------------------------------------------------------------

shouldAccept :: [License]
shouldAccept = map License
    [ simpleLicenseExpression GPL_2_0_only
    , simpleLicenseExpression GPL_2_0_or_later
    , simpleLicenseExpression BSD_2_Clause
    , simpleLicenseExpression BSD_3_Clause
    , simpleLicenseExpression MIT
    , simpleLicenseExpression ISC
    , simpleLicenseExpression MPL_2_0
    , simpleLicenseExpression Apache_2_0
    , simpleLicenseExpression CC0_1_0
    , simpleLicenseExpression BSD_4_Clause `EOr` simpleLicenseExpression MIT
    ]

shouldReject :: [License]
shouldReject = map License
    [ simpleLicenseExpression BSD_4_Clause
    , simpleLicenseExpression BSD_4_Clause `EAnd` simpleLicenseExpression MIT
    ]

-- | A sketch of what Hackage could accept
--
-- * NONE is rejected
--
-- * "or later" syntax (+ postfix) is rejected
--
-- * "WITH exc" exceptions are rejected
--
-- * There should be a way to interpert license as (conjunction of)
--   OSI-accepted licenses or CC0
--
isAcceptableLicense :: License -> Bool
isAcceptableLicense NONE           = False
isAcceptableLicense (License expr) = goExpr expr
  where
    goExpr (EAnd a b)            = goExpr a && goExpr b
    goExpr (EOr a b)             = goExpr a || goExpr b
    goExpr (ELicense _ (Just _)) = False -- Don't allow exceptions
    goExpr (ELicense s Nothing)  = goSimple s

    goSimple (ELicenseRef _)      = False -- don't allow referenced licenses
    goSimple (ELicenseIdPlus _)   = False -- don't allow + licenses (use GPL-3.0-or-later e.g.)
    goSimple (ELicenseId CC0_1_0) = True -- CC0 isn't OSI approved, but we allow it as "PublicDomain", this is eg. PublicDomain in http://hackage.haskell.org/package/string-qq-0.0.2/src/LICENSE
    goSimple (ELicenseId lid)     = licenseIsOsiApproved lid -- allow only OSI approved licenses.

shouldAcceptProp :: Property
shouldAcceptProp = conjoin $
    map (\l -> counterexample (prettyShow l) (isAcceptableLicense l)) shouldAccept

shouldRejectProp :: Property
shouldRejectProp = conjoin $
    map (\l -> counterexample (prettyShow l) (not $ isAcceptableLicense l)) shouldReject

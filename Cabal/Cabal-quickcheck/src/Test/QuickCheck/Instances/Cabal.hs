{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Cabal () where

import Control.Applicative        (liftA2)
import Data.Char                  (isAlphaNum, isDigit)
import Data.List                  (intercalate)
import Distribution.Utils.Generic (lowercase)
import Test.QuickCheck

import GHC.Generics

import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.Parsec.Newtypes
import Distribution.Simple.Flag                    (Flag (..))
import Distribution.SPDX
import Distribution.System
import Distribution.Types.Dependency
import Distribution.Types.Flag                     (FlagAssignment, FlagName, mkFlagAssignment, mkFlagName, unFlagAssignment)
import Distribution.Types.LibraryName
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.SourceRepo
import Distribution.Types.UnqualComponentName
import Distribution.Types.VersionRange.Internal
import Distribution.Verbosity
import Distribution.Version

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<*>))
#endif

-------------------------------------------------------------------------------
-- CabalSpecVersion
-------------------------------------------------------------------------------

instance Arbitrary CabalSpecVersion where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SpecVersion where
    arbitrary = fmap SpecVersion arbitrary

-------------------------------------------------------------------------------
-- PackageName and PackageIdentifier
-------------------------------------------------------------------------------

instance Arbitrary PackageName where
    arbitrary = mkPackageName . intercalate "-" <$> shortListOf1 2 nameComponent
      where
        nameComponent = shortListOf1 5 (elements packageChars)
                        `suchThat` (not . all isDigit)
        packageChars  = filter isAlphaNum ['\0'..'\127']

instance Arbitrary PackageIdentifier where
    arbitrary = PackageIdentifier <$> arbitrary <*> arbitrary

    shrink (PackageIdentifier pn vr) = uncurry PackageIdentifier <$> shrink (pn, vr)

-------------------------------------------------------------------------------
-- Version
-------------------------------------------------------------------------------

-- | Does *NOT* generate 'nullVersion'
instance Arbitrary Version where
  arbitrary = do
      branch <- smallListOf1 $
                  frequency [(3, return 0)
                            ,(3, return 1)
                            ,(2, return 2)
                            ,(2, return 3)
                            ,(1, return 0xfffd)
                            ,(1, return 0xfffe) -- max fitting into packed W64
                            ,(1, return 0xffff)
                            ,(1, return 999999998)
                            ,(1, return 999999999)
                            ,(1, return 0x10000)]
      return (mkVersion branch)
    where
      smallListOf1 = scale (\n -> min 6 (n `div` 3)) . listOf1

  shrink ver = [ mkVersion ns | ns <- shrink (versionNumbers ver)
                              , not (null ns) ]

instance Arbitrary VersionRange where
  arbitrary = sized verRangeExp
    where
      verRangeExp n = frequency $
        [ (2, return anyVersion)
        , (1, fmap thisVersion arbitrary)
        , (1, fmap laterVersion arbitrary)
        , (1, fmap orLaterVersion arbitrary)
        , (1, fmap orLaterVersion' arbitrary)
        , (1, fmap earlierVersion arbitrary)
        , (1, fmap orEarlierVersion arbitrary)
        , (1, fmap orEarlierVersion' arbitrary)
        , (1, fmap withinVersion arbitraryV)
        , (1, fmap majorBoundVersion arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftA2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftA2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      arbitraryV :: Gen Version
      arbitraryV = arbitrary `suchThat` \v -> all (< 999999999) (versionNumbers v)

      orLaterVersion'   v =
        unionVersionRanges (LaterVersion v)   (ThisVersion v)
      orEarlierVersion' v =
        unionVersionRanges (EarlierVersion v) (ThisVersion v)

  shrink (ThisVersion v)              = map ThisVersion (shrink v)
  shrink (LaterVersion v)             = map LaterVersion (shrink v)
  shrink (EarlierVersion v)           = map EarlierVersion (shrink v)
  shrink (OrLaterVersion v)           = LaterVersion v : map OrLaterVersion (shrink v)
  shrink (OrEarlierVersion v)         = EarlierVersion v : map OrEarlierVersion (shrink v)
  shrink (MajorBoundVersion v)        = map MajorBoundVersion (shrink v)
  shrink (UnionVersionRanges a b)     = a : b : map (uncurry UnionVersionRanges) (shrink (a, b))
  shrink (IntersectVersionRanges a b) = a : b : map (uncurry IntersectVersionRanges) (shrink (a, b))

-- | Generating VersionIntervals
--
-- This is a tad tricky as VersionIntervals is an abstract type, so we first
-- make a local type for generating the internal representation. Then we check
-- that this lets us construct valid 'VersionIntervals'.
--

instance Arbitrary VersionIntervals where
  arbitrary = fmap mkVersionIntervals' arbitrary
    where
      mkVersionIntervals' :: [(Version, Bound)] -> VersionIntervals
      mkVersionIntervals' = mkVersionIntervals . go version0
        where
          go :: Version -> [(Version, Bound)] -> [VersionInterval]
          go _ [] = []
          go v [(lv, lb)] =
              [(LowerBound (addVersion lv v) lb, NoUpperBound)]
          go v ((lv, lb) : (uv, ub) : rest) =
              (LowerBound lv' lb, UpperBound uv' ub) : go uv' rest
            where
              lv' = addVersion v lv
              uv' = addVersion lv' uv

          addVersion :: Version -> Version -> Version
          addVersion xs ys = mkVersion $  z (versionNumbers xs) (versionNumbers ys)
            where
              z [] ys' = ys'
              z xs' [] = xs'
              z (x : xs') (y : ys') = x + y : z xs' ys'

instance Arbitrary Bound where
  arbitrary = elements [ExclusiveBound, InclusiveBound]

-------------------------------------------------------------------------------
-- ModuleName
-------------------------------------------------------------------------------

instance Arbitrary ModuleName where
  arbitrary = fromString . intercalate "." <$> shortListOf1 4 comp where
    comp = (:) <$> elements upper <*> shortListOf1 10 (elements moduleChar)
    upper = ['A'..'Z']
    moduleChar = [ c | c <- ['\0' .. '\255'], isAlphaNum c || c `elem` "_'" ]

-------------------------------------------------------------------------------
-- Dependency
-------------------------------------------------------------------------------

instance Arbitrary Dependency where
    arbitrary = mkDependency
        <$> arbitrary
        <*> arbitrary
        <*> (arbitrary `suchThat` const True) -- should be (not . null)

    shrink (Dependency pn vr lb) =
        [ mkDependency pn' vr' lb'
        | (pn', vr', lb') <- shrink (pn, vr, lb)
        ]

-------------------------------------------------------------------------------
-- PackageVersionConstraint
-------------------------------------------------------------------------------

instance Arbitrary PackageVersionConstraint where
    arbitrary = PackageVersionConstraint
        <$> arbitrary
        <*> arbitrary

    shrink (PackageVersionConstraint pn vr) =
        [ PackageVersionConstraint pn' vr'
        | (pn', vr') <- shrink (pn, vr)
        ]

-------------------------------------------------------------------------------
-- System
-------------------------------------------------------------------------------

instance Arbitrary OS where
    arbitrary = elements knownOSs

instance Arbitrary Arch where
    arbitrary = elements knownArches

instance Arbitrary Platform where
    arbitrary = Platform <$> arbitrary <*> arbitrary

-------------------------------------------------------------------------------
-- Various names
-------------------------------------------------------------------------------

instance Arbitrary UnqualComponentName where
    -- same rules as package names
    arbitrary = packageNameToUnqualComponentName <$> arbitrary

instance Arbitrary LibraryName where
    arbitrary = oneof
        [ LSubLibName <$> arbitrary
        , pure LMainLibName
        ]

    shrink (LSubLibName _) = [LMainLibName]
    shrink _               = []

-------------------------------------------------------------------------------
-- option flags
-------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Flag a) where
    arbitrary = arbitrary1

    shrink NoFlag   = []
    shrink (Flag x) = NoFlag : [ Flag x' | x' <- shrink x ]

instance Arbitrary1 Flag where
    liftArbitrary genA = sized $ \sz ->
        if sz <= 0
        then pure NoFlag
        else frequency [ (1, pure NoFlag)
                       , (3, Flag <$> genA) ]

-------------------------------------------------------------------------------
-- GPD flags
-------------------------------------------------------------------------------

instance Arbitrary FlagName where
    arbitrary = mkFlagName <$> frequency
        [ (20, flagident)
        -- special nasty cases
        , (1,  pure "none")
        , (1,  pure "any")
        ]
      where
        flagident   = lowercase <$> shortListOf1 5 (elements flagChars)
                      `suchThat` (("-" /=) . take 1)
        flagChars   = "-_" ++ ['a'..'z']

instance Arbitrary FlagAssignment where
    arbitrary = mkFlagAssignment <$> arbitrary
    shrink x = mkFlagAssignment <$> shrink (unFlagAssignment x)

-------------------------------------------------------------------------------
-- Verbosity
-------------------------------------------------------------------------------

instance Arbitrary Verbosity where
    arbitrary = elements [minBound..maxBound]

-------------------------------------------------------------------------------
-- SourceRepo
-------------------------------------------------------------------------------

instance Arbitrary RepoType where
    arbitrary = elements (KnownRepoType <$> knownRepoTypes)

instance Arbitrary RepoKind where
    arbitrary = elements [RepoHead, RepoThis]

-------------------------------------------------------------------------------
-- SPDX
-------------------------------------------------------------------------------

instance Arbitrary LicenseId where
    arbitrary = elements $ licenseIdList LicenseListVersion_3_6

instance Arbitrary LicenseExceptionId where
    arbitrary = elements $ licenseExceptionIdList LicenseListVersion_3_6

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

-------------------------------------------------------------------------------
-- Compiler
-------------------------------------------------------------------------------

instance Arbitrary CompilerFlavor where
    arbitrary = elements knownCompilerFlavors

instance Arbitrary CompilerId where
    arbitrary = genericArbitrary
    shrink    = genericShrink

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 bound gen = sized $ \n -> do
    k <- choose (1, 1 `max` ((n `div` 2) `min` bound))
    vectorOf k gen

-------------------------------------------------------------------------------
-- Generic Arbitrary
-------------------------------------------------------------------------------

-- Generic arbitary for non-recursive types
genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = fmap to garbitrary

class GArbitrary f where
    garbitrary :: Gen (f ())

class GArbitrarySum f where
    garbitrarySum :: [Gen (f ())]

class GArbitraryProd f where
    garbitraryProd :: Gen (f ())

instance (GArbitrarySum f, i ~ D) => GArbitrary (M1 i c f) where
    garbitrary = fmap M1 (oneof garbitrarySum)

instance (GArbitraryProd f, i ~ C) => GArbitrarySum (M1 i c f) where
    garbitrarySum = [fmap M1 garbitraryProd]

instance (GArbitrarySum f, GArbitrarySum g) => GArbitrarySum (f :+: g) where
    garbitrarySum = map (fmap L1) garbitrarySum ++ map (fmap R1) garbitrarySum

instance (GArbitraryProd f, i ~ S) => GArbitraryProd (M1 i c f) where
    garbitraryProd = fmap M1 garbitraryProd

instance (GArbitraryProd f, GArbitraryProd g) => GArbitraryProd (f :*: g) where
    garbitraryProd = liftA2 (:*:) garbitraryProd garbitraryProd

instance (Arbitrary a) => GArbitraryProd (K1 i a) where
    garbitraryProd = fmap K1 arbitrary

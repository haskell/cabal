{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
module Distribution.Arbitrary.Instances () where

import Control.Monad
  ( liftM
  , liftM2
  )
import Data.Char
  ( isAlphaNum
  , isDigit
  )
import Data.List
  ( intercalate
  )
import Distribution.Simple.Flag
  ( Flag (..)
  )
import Distribution.Simple.InstallDirs
  ( PathTemplate
  , toPathTemplate
  )
import Distribution.Simple.Utils
  ( lowercase
  )
import Distribution.SPDX
  ( LicenseId
  , LicenseExceptionId
  , LicenseExpression (..)
  , LicenseListVersion (..)
  , LicenseRef
  , SimpleLicenseExpression (..)
  , licenseExceptionIdList
  , licenseIdList
  , mkLicenseRef'
  )
import Distribution.System
  ( Arch
  , OS
  , Platform (..)
  , knownArches
  , knownOSs
  )
import Distribution.Types.Dependency
  ( Dependency (..)
  )
import Distribution.Types.GenericPackageDescription
  ( FlagName
  , mkFlagName
  )
import Distribution.Types.LibraryName
  ( LibraryName (..)
  )
import Distribution.Types.PackageName
  ( PackageName
  , mkPackageName
  )
import Distribution.Types.PackageVersionConstraint
  ( PackageVersionConstraint (..)
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , packageNameToUnqualComponentName
  )
import Distribution.Verbosity
  ( Verbosity
  )
import Distribution.Version
  ( Bound (..)
  , LowerBound (..)
  , UpperBound (..)
  , Version
  , VersionInterval
  , VersionIntervals
  , VersionRange (..)
  , anyVersion
  , earlierVersion
  , intersectVersionRanges
  , laterVersion
  , majorBoundVersion
  , mkVersion
  , mkVersionIntervals
  , orEarlierVersion
  , orLaterVersion
  , thisVersion
  , unionVersionRanges
  , version0
  , versionNumbers
  , withinVersion
  )
import Test.QuickCheck
  ( Arbitrary ( arbitrary, shrink )
  , elements
  , frequency
  , listOf1
  , oneof
  , sized
  , suchThat
  )

import Distribution.Arbitrary.Util

-- Instances from Distribution.Simple.Flag

instance Arbitrary a => Arbitrary (Flag a) where
    arbitrary = arbitraryFlag arbitrary
    shrink NoFlag   = []
    shrink (Flag x) = NoFlag : [ Flag x' | x' <- shrink x ]

-- Instances from Distribution.Simple.InstallDirs

instance Arbitrary PathTemplate where
    arbitrary = toPathTemplate <$> arbitraryShortToken
    shrink t  = [ toPathTemplate s | s <- shrink (show t), not (null s) ]

-- Instances from Distribution.System

instance Arbitrary Arch where
    arbitrary = elements knownArches

instance Arbitrary OS where
    arbitrary = elements knownOSs

instance Arbitrary Platform where
    arbitrary = liftM2 Platform arbitrary arbitrary

-- Instances from Distribution.Types.Dependency

instance Arbitrary Dependency where
    arbitrary = Dependency <$> arbitrary <*> arbitrary <*> fmap getNonMEmpty arbitrary

-- Instances from Distribution.Types.GenericPackageDescription

instance Arbitrary FlagName where
    arbitrary = mkFlagName <$> flagident
      where
        flagident   = lowercase <$> shortListOf1 5 (elements flagChars)
                      `suchThat` (("-" /=) . take 1)
        flagChars   = "-_" ++ ['a'..'z']

-- Instances from Distribution.Types.LibraryName

instance Arbitrary LibraryName where
    arbitrary = elements =<< sequenceA [LSubLibName <$> arbitrary, pure LMainLibName]

-- Instances from Distribution.Types.PackageName

instance Arbitrary PackageName where
    arbitrary = mkPackageName . intercalate "-" <$> shortListOf1 2 nameComponent
      where
        nameComponent = shortListOf1 5 (elements packageChars)
                        `suchThat` (not . all isDigit)
        packageChars  = filter isAlphaNum ['\0'..'\127']

-- Instances from Distribution.Types.PackageVersionConstraint

instance Arbitrary PackageVersionConstraint where
    arbitrary = PackageVersionConstraint <$> arbitrary <*> arbitrary

-- Instances from Distribution.Types.UnqualComponentName

instance Arbitrary UnqualComponentName where
    -- same rules as package names
    arbitrary = packageNameToUnqualComponentName <$> arbitrary

-- Instances from Distribution.Verbosity

instance Arbitrary Verbosity where
    arbitrary = elements [minBound..maxBound]

-- Instances from Distribution.Version

instance Arbitrary Bound where
  arbitrary = elements [ExclusiveBound, InclusiveBound]

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
                            ,(1, return 0x10000)]
      return (mkVersion branch)
    where
      smallListOf1 = adjustSize (\n -> min 6 (n `div` 3)) . listOf1

  shrink ver = [ mkVersion ns | ns <- shrink (versionNumbers ver)
                              , not (null ns) ]

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

instance Arbitrary VersionRange where
  arbitrary = sized verRangeExp
    where
      verRangeExp n = frequency $
        [ (2, return anyVersion)
        , (1, liftM thisVersion arbitrary)
        , (1, liftM laterVersion arbitrary)
        , (1, liftM orLaterVersion arbitrary)
        , (1, liftM orLaterVersion' arbitrary)
        , (1, liftM earlierVersion arbitrary)
        , (1, liftM orEarlierVersion arbitrary)
        , (1, liftM orEarlierVersion' arbitrary)
        , (1, liftM withinVersion arbitrary)
        , (1, liftM majorBoundVersion arbitrary)
        , (2, liftM VersionRangeParens arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      orLaterVersion'   v =
        unionVersionRanges (LaterVersion v)   (ThisVersion v)
      orEarlierVersion' v =
        unionVersionRanges (EarlierVersion v) (ThisVersion v)

  shrink AnyVersion                   = []
  shrink (ThisVersion v)              = map ThisVersion (shrink v)
  shrink (LaterVersion v)             = map LaterVersion (shrink v)
  shrink (EarlierVersion v)           = map EarlierVersion (shrink v)
  shrink (OrLaterVersion v)           = LaterVersion v : map OrLaterVersion (shrink v)
  shrink (OrEarlierVersion v)         = EarlierVersion v : map OrEarlierVersion (shrink v)
  shrink (WildcardVersion v)          = map WildcardVersion ( shrink v)
  shrink (MajorBoundVersion v)        = map MajorBoundVersion (shrink v)
  shrink (VersionRangeParens vr)      = vr : map VersionRangeParens (shrink vr)
  shrink (UnionVersionRanges a b)     = a : b : map (uncurry UnionVersionRanges) (shrink (a, b))
  shrink (IntersectVersionRanges a b) = a : b : map (uncurry IntersectVersionRanges) (shrink (a, b))

-- Instances from Distribution.SPDX

instance Arbitrary LicenseId where
    arbitrary = elements $ licenseIdList LicenseListVersion_3_2

instance Arbitrary LicenseExceptionId where
    arbitrary = elements $ licenseExceptionIdList LicenseListVersion_3_2

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

{-# LANGUAGE CPP           #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.Instances.Cabal () where

import Control.Applicative        (liftA2)
import Data.Bits                  (shiftR)
import Data.Char                  (isAlphaNum, isDigit, toLower)
import Data.List                  (intercalate, (\\))
import Data.List.NonEmpty         (NonEmpty (..))
import Distribution.Utils.Generic (lowercase)
import Test.QuickCheck

#if MIN_VERSION_base(4,8,0)
import Data.Bits (countLeadingZeros, finiteBitSize, shiftL)
#else
import Data.Bits (popCount)
#endif

import Distribution.CabalSpecVersion
import Distribution.Compat.NonEmptySet             (NonEmptySet)
import Distribution.Compiler
import Distribution.FieldGrammar.Newtypes
import Distribution.ModuleName
import Distribution.Simple.Compiler                (DebugInfoLevel (..), OptimisationLevel (..), PackageDB (..), ProfDetailLevel (..), knownProfDetailLevels)
import Distribution.Simple.Flag                    (Flag (..))
import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup                   (HaddockTarget (..), TestShowDetails (..), DumpBuildInfo)
import Distribution.SPDX
import Distribution.System
import Distribution.Types.Dependency
import Distribution.Types.Flag                     (FlagAssignment, FlagName, mkFlagAssignment, mkFlagName, unFlagAssignment)
import Distribution.Types.IncludeRenaming
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.Mixin
import Distribution.Types.ModuleRenaming
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.SourceRepo
import Distribution.Types.UnqualComponentName
import Distribution.Types.VersionRange.Internal
import Distribution.Utils.NubList
import Distribution.Verbosity
import Distribution.Version

import Test.QuickCheck.GenericArbitrary

import qualified Data.ByteString.Char8           as BS8
import qualified Distribution.Compat.NonEmptySet as NES

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
                        `suchThat` (liftA2 (&&) (not . all isDigit) (/= "all"))
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
  arbitrary = sized $ \n -> chooseInt (0, n) >>= verRangeExp . intSqrt
    where
      verRangeExp n
        | n > 0     = oneof
          [ recurse unionVersionRanges     n
          , recurse intersectVersionRanges n
          ]
        | otherwise = oneof
          [ return anyVersion
          , fmap thisVersion arbitrary
          , fmap laterVersion arbitrary
          , fmap orLaterVersion arbitrary
          , fmap orLaterVersion' arbitrary
          , fmap earlierVersion arbitrary
          , fmap orEarlierVersion arbitrary
          , fmap orEarlierVersion' arbitrary
          , fmap withinVersion arbitraryV
          , fmap majorBoundVersion arbitrary
          ]

      recurse mk n = do
        k <- chooseInt (0, n - 1)
        liftA2 mk (verRangeExp k) (verRangeExp (n - k - 1))

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

instance Arbitrary VersionIntervals where
  arbitrary = fmap toVersionIntervals arbitrary

instance Arbitrary Bound where
  arbitrary = elements [ExclusiveBound, InclusiveBound]

-------------------------------------------------------------------------------
-- Backpack
-------------------------------------------------------------------------------

instance Arbitrary Mixin where
    arbitrary = normaliseMixin <$> genericArbitrary
    shrink    = fmap normaliseMixin . genericShrink

instance Arbitrary IncludeRenaming where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary ModuleRenaming where
    arbitrary = genericArbitrary
    shrink    = genericShrink

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

instance Arbitrary LibraryVisibility where
  arbitrary = elements [LibraryVisibilityPrivate, LibraryVisibilityPublic]

  shrink LibraryVisibilityPublic  = [LibraryVisibilityPrivate]
  shrink LibraryVisibilityPrivate = []

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
    arbitrary = do
        v <- elements [minBound..maxBound]
        -- verbose markoutput is left out on purpose
        flags <- listOf $ elements
            [ verboseCallSite
            , verboseCallStack
            , verboseNoWrap
            , verboseTimestamp
            , verboseStderr
            ]
        return (foldr ($) v flags)

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
    arbitrary = elements $ licenseIdList currentLicenseListVersion

instance Arbitrary LicenseExceptionId where
    arbitrary = elements $ licenseExceptionIdList currentLicenseListVersion

currentLicenseListVersion :: LicenseListVersion
currentLicenseListVersion = cabalSpecVersionToSPDXListVersion cabalSpecLatest

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

instance Arbitrary ProfDetailLevel where
    arbitrary = elements [ d | (_,_,d) <- knownProfDetailLevels ]

instance Arbitrary OptimisationLevel where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary DebugInfoLevel where
    arbitrary = elements [minBound..maxBound]

-------------------------------------------------------------------------------
-- NonEmptySet
-------------------------------------------------------------------------------

instance (Arbitrary a, Ord a) => Arbitrary (NonEmptySet a) where
    arbitrary = mk <$> arbitrary <*> arbitrary where
        mk x xs = NES.fromNonEmpty (x :| xs)

    shrink nes = case NES.toNonEmpty nes of
        x :| xs -> map mk (shrink (x, xs))
      where
        mk (x,xs) = NES.fromNonEmpty (x :| xs)

-------------------------------------------------------------------------------
-- NubList
-------------------------------------------------------------------------------

instance (Arbitrary a, Ord a) => Arbitrary (NubList a) where
    arbitrary = toNubList <$> arbitrary
    shrink xs = [ toNubList [] | (not . null) (fromNubList xs) ]
    -- try empty, otherwise don't shrink as it can loop

-------------------------------------------------------------------------------
-- InstallDirs
-------------------------------------------------------------------------------

-- these are wrong because they bottom out in String. We should really use
-- the modern FilePath at some point, so we get QC instances that don't include
-- invalid characters or path components

instance Arbitrary a => Arbitrary (InstallDirs a) where
    arbitrary = InstallDirs
        <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  4
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary --  8
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 12
        <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary -- 16

instance Arbitrary PathTemplate where
    arbitrary = toPathTemplate <$> arbitraryShortToken
    shrink t  = [ toPathTemplate s
                | s <- shrink (fromPathTemplate t)
                , not (null s) ]

-------------------------------------------------------------------------------
-- Pkgconfig
-------------------------------------------------------------------------------

instance Arbitrary PkgconfigVersion where
    arbitrary = PkgconfigVersion . BS8.pack . dropDash . concat <$> listOf1 elems where
        elems = frequency
            [ (2, pure ".")
            , (1, pure "-")
            , (5, listOf1 $ elements ['0' .. '9'])
            , (1, listOf1 $ elements ['A' .. 'Z'])
            , (1, listOf1 $ elements ['a' .. 'z'])
            ]

        -- disallow versions starting with dash
        dropDash = notEmpty . dropWhile (== '-')
        notEmpty x
            | null x    = "0"
            | otherwise = x

instance Arbitrary PkgconfigVersionRange where
  arbitrary = sized $ \n -> chooseInt (0, n) >>= verRangeExp . intSqrt
    where
      verRangeExp n
        | n > 0     = oneof
          [ recurse PcUnionVersionRanges     n
          , recurse PcIntersectVersionRanges n
          ]
        | otherwise = oneof
          [ return PcAnyVersion
          , fmap PcThisVersion arbitrary
          , fmap PcLaterVersion arbitrary
          , fmap PcOrLaterVersion arbitrary
          , fmap orLaterVersion' arbitrary
          , fmap PcEarlierVersion arbitrary
          , fmap PcOrEarlierVersion arbitrary
          , fmap orEarlierVersion' arbitrary
          ]

      recurse mk n = do
        k <- chooseInt (0, n - 1)
        liftA2 mk (verRangeExp k) (verRangeExp (n - k - 1))

      orLaterVersion'   v =
        PcUnionVersionRanges (PcLaterVersion v)   (PcThisVersion v)
      orEarlierVersion' v =
        PcUnionVersionRanges (PcEarlierVersion v) (PcThisVersion v)

-------------------------------------------------------------------------------
-- Setup
-------------------------------------------------------------------------------

instance Arbitrary HaddockTarget where
    arbitrary = elements [ForHackage, ForDevelopment]

instance Arbitrary TestShowDetails where
    arbitrary = arbitraryBoundedEnum

-------------------------------------------------------------------------------
-- PackageDB
-------------------------------------------------------------------------------

instance Arbitrary PackageDB where
    arbitrary = oneof [ pure GlobalPackageDB
                      , pure UserPackageDB
                      , SpecificPackageDB <$> arbitraryShortPath
                      ]

-------------------------------------------------------------------------------
-- DumpBuildInfo
-------------------------------------------------------------------------------

instance Arbitrary DumpBuildInfo where
    arbitrary = arbitraryBoundedEnum

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 bound gen = sized $ \n -> do
    k <- choose (1, 1 `max` ((n `div` 2) `min` bound))
    vectorOf k gen

arbitraryShortToken :: Gen String
arbitraryShortToken = arbitraryShortStringWithout "{}[]"

arbitraryShortPath :: Gen String
arbitraryShortPath = arbitraryShortStringWithout "{}[],<>:|*?" `suchThat` (not . winDevice)
    where
        -- split path components on dots
        -- no component can be empty or a device name
        -- this blocks a little too much (both "foo..bar" and "foo.con" are legal)
        -- but for QC being a little conservative isn't harmful
        winDevice = any (any (`elem` ["","con", "aux", "prn", "com", "lpt", "nul"]) . splitBy ".") . splitBy "\\/" . map toLower
        splitBy _ "" = []
        splitBy seps str = let (part,rest) = break (`elem` seps) str
                            in part : if length rest == 1 then [""] else splitBy seps (drop 1 rest)

arbitraryShortStringWithout :: String -> Gen String
arbitraryShortStringWithout excludeChars =
    shortListOf1 5 $ elements $ ['#' .. '~'] \\ excludeChars

-- |
intSqrt :: Int -> Int
intSqrt 0 = 0
intSqrt 1 = 1
intSqrt n = case compare n 0 of
    LT -> 0 -- whatever
    EQ -> 0
    GT -> iter (iter guess) -- two iterations give good results
  where
    iter :: Int -> Int
    iter 0 = 0
    iter x = shiftR (x + n `div` x) 1

    guess :: Int
#if MIN_VERSION_base(4,8,0)
    guess = shiftR n (shiftL (finiteBitSize n - countLeadingZeros n) 1)
#else
    guess = shiftR n (shiftR (popCount n) 1)
#endif

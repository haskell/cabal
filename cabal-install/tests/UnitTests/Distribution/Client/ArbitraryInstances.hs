{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ArbitraryInstances (
    adjustSize,
    shortListOf1,
    arbitraryShortToken
  ) where

import Control.Monad
import Control.Applicative

import Distribution.Version
import Distribution.Package
import Distribution.System
import Distribution.Verbosity

import Distribution.Simple.Setup
import Distribution.Simple.InstallDirs

import Distribution.Utils.NubList


import Test.QuickCheck


adjustSize :: (Int -> Int) -> Gen a -> Gen a
adjustSize adjust gen = sized (\n -> resize (adjust n) gen)

shortListOf1 :: Gen a -> Gen [a]
shortListOf1 = adjustSize (\n -> min 5 (n `div` 3)) . listOf1

arbitraryShortToken :: Gen String
arbitraryShortToken = shortListOf1 (choose ('#', '~'))


instance Arbitrary Version where
  arbitrary = do
    branch <- shortListOf1 $
                frequency [(3, return 0)
                          ,(3, return 1)
                          ,(2, return 2)
                          ,(1, return 3)]
    return (Version branch []) -- deliberate []
    where

  shrink (Version branch []) =
    [ Version branch' [] | branch' <- shrink branch, not (null branch') ]
  shrink (Version branch _tags) =
    [ Version branch [] ]

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
        , (2, liftM VersionRangeParens arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      orLaterVersion'   v =
        unionVersionRanges (laterVersion v)   (thisVersion v)
      orEarlierVersion' v =
        unionVersionRanges (earlierVersion v) (thisVersion v)

instance Arbitrary PackageName where
    arbitrary = PackageName <$> arbitraryShortToken

instance Arbitrary Dependency where
    arbitrary = Dependency <$> arbitrary <*> arbitrary

instance Arbitrary OS where
    arbitrary = elements knownOSs

instance Arbitrary Arch where
    arbitrary = elements knownArches

instance Arbitrary Platform where
    arbitrary = Platform <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Flag a) where
    arbitrary = frequency [ (1, pure NoFlag)
                          , (3, Flag <$> arbitrary) ]

instance (Arbitrary a, Ord a) => Arbitrary (NubList a) where
    arbitrary = toNubList <$> arbitrary

instance Arbitrary Verbosity where
    arbitrary = elements [minBound..maxBound]

instance Arbitrary PathTemplate where
    arbitrary = toPathTemplate <$> arbitraryShortToken


{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ArbitraryInstances (
    NoShrink(..),
    CanonicalPackageVersionConstraint(..),
    CanonicalVersionRange(..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
import Control.Applicative
#endif

import Distribution.Types.PackageVersionConstraint
import Distribution.Utils.NubList
import Distribution.Version

import Distribution.Client.IndexUtils.Timestamp

import Distribution.Arbitrary.Instances ()

import Test.QuickCheck


instance (Arbitrary a, Ord a) => Arbitrary (NubList a) where
    arbitrary = toNubList <$> arbitrary
    shrink xs = [ toNubList [] | (not . null) (fromNubList xs) ]
    -- try empty, otherwise don't shrink as it can loop

newtype NoShrink a = NoShrink { getNoShrink :: a }
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (NoShrink a) where
    arbitrary = NoShrink <$> arbitrary
    shrink _  = []

instance Arbitrary Timestamp where
    arbitrary = (maybe (toEnum 0) id . epochTimeToTimestamp) <$> arbitrary

instance Arbitrary IndexState where
    arbitrary = frequency [ (1, pure IndexStateHead)
                          , (50, IndexStateTime <$> arbitrary)
                          ]


newtype CanonicalPackageVersionConstraint = CanonicalPackageVersionConstraint
  { getCanonicalPackageVersionConstraint :: PackageVersionConstraint }
  deriving (Show)

instance Arbitrary CanonicalPackageVersionConstraint where
  arbitrary = f <$> arbitrary <*> arbitrary
    where
      f pkgs range = CanonicalPackageVersionConstraint $
        PackageVersionConstraint pkgs (getCanonicalVersionRange range)

  shrink = fmap CanonicalPackageVersionConstraint . shrink . getCanonicalPackageVersionConstraint

newtype CanonicalVersionRange = CanonicalVersionRange
  { getCanonicalVersionRange :: VersionRange }
  deriving (Show)

instance Arbitrary CanonicalVersionRange where
  arbitrary = fmap canonicalise arbitrary
    where
      canonicalise = CanonicalVersionRange . fromVersionIntervals . toVersionIntervals

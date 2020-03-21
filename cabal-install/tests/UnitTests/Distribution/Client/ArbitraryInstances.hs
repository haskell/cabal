{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Client.ArbitraryInstances (
    adjustSize,
    shortListOf,
    shortListOf1,
    arbitraryFlag,
    ShortToken(..),
    arbitraryShortToken,
    NonMEmpty(..),
    NoShrink(..),
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageVersionConstraint

import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup

import Distribution.Utils.NubList

import Distribution.Client.BuildReports.Types            (ReportLevel (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (InstallMethod)
import Distribution.Client.IndexUtils.IndexState         (IndexState (..))
import Distribution.Client.IndexUtils.Timestamp          (Timestamp, epochTimeToTimestamp)
import Distribution.Client.InstallSymlink                (OverwritePolicy)
import Distribution.Client.Types                         (RepoName (..), WriteGhcEnvironmentFilesPolicy)

import Test.QuickCheck
import Test.QuickCheck.Instances.Cabal ()

import Network.URI (URI (..), URIAuth (..), isUnreserved)

-- note: there are plenty of instances defined in ProjectConfig test file.
-- they should be moved here or into Cabal-quickcheck

-------------------------------------------------------------------------------
-- Non-Cabal instances
-------------------------------------------------------------------------------

instance Arbitrary URI where
    arbitrary =
      URI <$> elements ["file:", "http:", "https:"]
          <*> (Just <$> arbitrary)
          <*> (('/':) <$> arbitraryURIToken)
          <*> (('?':) <$> arbitraryURIToken)
          <*> pure ""

instance Arbitrary URIAuth where
    arbitrary =
      URIAuth <$> pure ""   -- no password as this does not roundtrip
              <*> arbitraryURIToken
              <*> arbitraryURIPort

arbitraryURIToken :: Gen String
arbitraryURIToken =
    shortListOf1 6 (elements (filter isUnreserved ['\0'..'\255']))

arbitraryURIPort :: Gen String
arbitraryURIPort =
    oneof [ pure "", (':':) <$> shortListOf1 4 (choose ('0','9')) ]

-------------------------------------------------------------------------------
-- cabal-install (and Cabal) types
-------------------------------------------------------------------------------

adjustSize :: (Int -> Int) -> Gen a -> Gen a
adjustSize adjust gen = sized (\n -> resize (adjust n) gen)

shortListOf :: Int -> Gen a -> Gen [a]
shortListOf bound gen =
    sized $ \n -> do
      k <- choose (0, (n `div` 2) `min` bound)
      vectorOf k gen

shortListOf1 :: Int -> Gen a -> Gen [a]
shortListOf1 bound gen =
    sized $ \n -> do
      k <- choose (1, 1 `max` ((n `div` 2) `min` bound))
      vectorOf k gen

newtype ShortToken = ShortToken { getShortToken :: String }
  deriving Show

instance Arbitrary ShortToken where
  arbitrary =
    ShortToken <$>
      (shortListOf1 5 (choose ('#', '~'))
       `suchThat` (not . ("[]" `isPrefixOf`)))
    --TODO: [code cleanup] need to replace parseHaskellString impl to stop
    -- accepting Haskell list syntax [], ['a'] etc, just allow String syntax.
    -- Workaround, don't generate [] as this does not round trip.

  shrink (ShortToken cs) =
    [ ShortToken cs' | cs' <- shrink cs, not (null cs') ]

arbitraryShortToken :: Gen String
arbitraryShortToken = getShortToken <$> arbitrary

instance Arbitrary PackageVersionConstraint where
    arbitrary = PackageVersionConstraint <$> arbitrary <*> arbitrary

instance (Arbitrary a, Ord a) => Arbitrary (NubList a) where
    arbitrary = toNubList <$> arbitrary
    shrink xs = [ toNubList [] | (not . null) (fromNubList xs) ]
    -- try empty, otherwise don't shrink as it can loop


instance Arbitrary PathTemplate where
    arbitrary = toPathTemplate <$> arbitraryShortToken
    shrink t  = [ toPathTemplate s
                | s <- shrink (fromPathTemplate t)
                , not (null s) ]


newtype NonMEmpty a = NonMEmpty { getNonMEmpty :: a }
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Monoid a, Eq a) => Arbitrary (NonMEmpty a) where
  arbitrary = NonMEmpty <$> (arbitrary `suchThat` (/= mempty))
  shrink (NonMEmpty x) = [ NonMEmpty x' | x' <- shrink x, x' /= mempty ]

newtype NoShrink a = NoShrink { getNoShrink :: a }
  deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (NoShrink a) where
    arbitrary = NoShrink <$> arbitrary
    shrink _  = []

instance Arbitrary Timestamp where
    -- note: no negative timestamps
    --
    -- >>> utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 100000 01 01) 0
    -- >>> 3093527980800s
    --
    arbitrary = maybe (toEnum 0) id . epochTimeToTimestamp . (`mod` 3093527980800) . abs <$> arbitrary

instance Arbitrary IndexState where
    arbitrary = frequency [ (1, pure IndexStateHead)
                          , (50, IndexStateTime <$> arbitrary)
                          ]

instance Arbitrary WriteGhcEnvironmentFilesPolicy where
    arbitrary = arbitraryBoundedEnum

arbitraryFlag :: Gen a -> Gen (Flag a)
arbitraryFlag = liftArbitrary

instance Arbitrary RepoName where
    arbitrary = RepoName <$> listOf1 (elements
        [ c | c <- [ '\NUL' .. '\255' ], isAlphaNum c || c `elem` "_-."])

instance Arbitrary ReportLevel where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary OverwritePolicy where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InstallMethod where
    arbitrary = arbitraryBoundedEnum

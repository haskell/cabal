{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeOperators    #-}
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
    -- * Shrinker
    Shrinker,
    runShrinker,
    shrinker,
    shrinkerPP,
    shrinkerAla,
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.InstallDirs
import Distribution.Simple.Setup
import Distribution.Types.Flag         (mkFlagAssignment)

import Distribution.Utils.NubList

import Distribution.Client.BuildReports.Types            (BuildReport, InstallOutcome, Outcome, ReportLevel (..))
import Distribution.Client.CmdInstall.ClientInstallFlags (InstallMethod)
import Distribution.Client.IndexUtils.ActiveRepos        (ActiveRepoEntry (..), ActiveRepos (..), CombineStrategy (..))
import Distribution.Client.IndexUtils.IndexState         (RepoIndexState (..), TotalIndexState, makeTotalIndexState)
import Distribution.Client.IndexUtils.Timestamp          (Timestamp, epochTimeToTimestamp)
import Distribution.Client.InstallSymlink                (OverwritePolicy)
import Distribution.Client.Targets
import Distribution.Client.Types                         (RepoName (..), WriteGhcEnvironmentFilesPolicy)
import Distribution.Client.Types.AllowNewer
import Distribution.Solver.Types.OptionalStanza          (OptionalStanza (..))
import Distribution.Solver.Types.PackageConstraint       (PackageProperty (..))

import Data.Coerce                     (Coercible, coerce)
import GHC.Generics
import Network.URI                     (URI (..), URIAuth (..), isUnreserved)
import Test.QuickCheck
import Test.QuickCheck.Instances.Cabal ()

-- note: there are plenty of instances defined in ProjectConfig test file.
-- they should be moved here or into Cabal-quickcheck

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

data Shrinker a = Shrinker a [a]

instance Functor Shrinker where
    fmap f (Shrinker x xs) = Shrinker (f x) (map f xs)

instance Applicative Shrinker where
    pure x = Shrinker x []

    Shrinker f fs <*> Shrinker x xs = Shrinker (f x) (map f xs ++ map ($ x) fs)

runShrinker :: Shrinker a -> [a]
runShrinker (Shrinker _ xs) = xs

shrinker :: Arbitrary a => a -> Shrinker a
shrinker x = Shrinker x (shrink x)

shrinkerAla :: (Coercible a b, Arbitrary b) => (a -> b) -> a -> Shrinker a
shrinkerAla pack = shrinkerPP pack coerce

-- | shrinker with pre and post functions.
shrinkerPP :: Arbitrary b => (a -> b) -> (b -> a) -> a -> Shrinker a
shrinkerPP pack unpack x = Shrinker x (map unpack (shrink (pack x)))

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

shrinkBoundedEnum :: (Eq a, Enum a, Bounded a) => a -> [a]
shrinkBoundedEnum x
    | x == minBound = []
    | otherwise     = [pred x]

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

instance Arbitrary RepoIndexState where
    arbitrary = frequency [ (1, pure IndexStateHead)
                          , (50, IndexStateTime <$> arbitrary)
                          ]

instance Arbitrary TotalIndexState where
    arbitrary = makeTotalIndexState <$> arbitrary <*> arbitrary

instance Arbitrary WriteGhcEnvironmentFilesPolicy where
    arbitrary = arbitraryBoundedEnum

arbitraryFlag :: Gen a -> Gen (Flag a)
arbitraryFlag = liftArbitrary

instance Arbitrary RepoName where
    arbitrary = RepoName <$> mk where
      mk = (:) <$> lead <*> rest
      lead = elements
        [ c | c <- [ '\NUL' .. '\255' ], isAlpha c || c `elem` "_-."]
      rest = listOf (elements
        [ c | c <- [ '\NUL' .. '\255' ], isAlphaNum c || c `elem` "_-."])

instance Arbitrary ReportLevel where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary OverwritePolicy where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary InstallMethod where
    arbitrary = arbitraryBoundedEnum

-------------------------------------------------------------------------------
-- ActiveRepos
-------------------------------------------------------------------------------

instance Arbitrary ActiveRepos where
    arbitrary = ActiveRepos <$> shortListOf 5 arbitrary

instance Arbitrary ActiveRepoEntry where
    arbitrary = frequency
        [ (10, ActiveRepo <$> arbitrary <*> arbitrary)
        , (1, ActiveRepoRest <$> arbitrary)
        ]

instance Arbitrary CombineStrategy where
    arbitrary = arbitraryBoundedEnum
    shrink    = shrinkBoundedEnum

-------------------------------------------------------------------------------
-- AllowNewer
-------------------------------------------------------------------------------

instance Arbitrary AllowNewer where
    arbitrary = AllowNewer <$> arbitrary

instance Arbitrary AllowOlder where
    arbitrary = AllowOlder <$> arbitrary

instance Arbitrary RelaxDeps where
    arbitrary = oneof [ pure mempty
                      , mkRelaxDepSome <$> shortListOf1 3 arbitrary
                      , pure RelaxDepsAll
                      ]

instance Arbitrary RelaxDepMod where
    arbitrary = elements [RelaxDepModNone, RelaxDepModCaret]

instance Arbitrary RelaxDepScope where
    arbitrary = oneof [ pure RelaxDepScopeAll
                      , RelaxDepScopePackage <$> arbitrary
                      , RelaxDepScopePackageId <$> arbitrary
                      ]

instance Arbitrary RelaxDepSubject where
    arbitrary = oneof [ pure RelaxDepSubjectAll
                      , RelaxDepSubjectPkg <$> arbitrary
                      ]

instance Arbitrary RelaxedDep where
    arbitrary = RelaxedDep <$> arbitrary <*> arbitrary <*> arbitrary

-------------------------------------------------------------------------------
-- UserConstraint
-------------------------------------------------------------------------------

instance Arbitrary UserConstraintScope where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary UserQualifier where
    arbitrary = oneof [ pure UserQualToplevel
                      , UserQualSetup <$> arbitrary

                      -- -- TODO: Re-enable UserQualExe tests once we decide on a syntax.
                      -- , UserQualExe <$> arbitrary <*> arbitrary
                      ]


instance Arbitrary UserConstraint where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary PackageProperty where
    arbitrary = oneof [ PackagePropertyVersion <$> arbitrary
                      , pure PackagePropertyInstalled
                      , pure PackagePropertySource
                      , PackagePropertyFlags  . mkFlagAssignment <$> shortListOf1 3 arbitrary
                      , PackagePropertyStanzas . (\x->[x]) <$> arbitrary
                      ]

instance Arbitrary OptionalStanza where
    arbitrary = elements [minBound..maxBound]

-------------------------------------------------------------------------------
-- BuildReport
-------------------------------------------------------------------------------

instance Arbitrary BuildReport where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary InstallOutcome where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Outcome where
    arbitrary = genericArbitrary
    shrink    = genericShrink

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
    garbitraryProd = (:*:) <$> garbitraryProd <*> garbitraryProd

instance GArbitraryProd U1 where
    garbitraryProd = pure U1

instance (Arbitrary a) => GArbitraryProd (K1 i a) where
    garbitraryProd = fmap K1 arbitrary

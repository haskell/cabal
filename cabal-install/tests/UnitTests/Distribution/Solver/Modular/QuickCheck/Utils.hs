{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module UnitTests.Distribution.Solver.Modular.QuickCheck.Utils
  ( testPropertyWithSeed
  , ArbitraryOrd (..)
  ) where

import Data.Tagged (Tagged, retag)
import GHC.Generics
import System.Random (getStdRandom, random)

import qualified Test.QuickCheck as QC
import Test.Tasty (TestTree)
import Test.Tasty.Options (OptionDescription, lookupOption, setOption)
import Test.Tasty.Providers (IsTest (..), singleTest)
import Test.Tasty.QuickCheck
  ( QC (..)
  , QuickCheckReplay (..)
  , Testable
  , property
  )

import Distribution.Simple.Utils
import Distribution.Verbosity

-- | Create a QuickCheck test that prints the seed before testing the property.
-- The seed can be useful for debugging non-terminating test cases. This is
-- related to https://github.com/feuerbach/tasty/issues/86.
testPropertyWithSeed :: Testable a => String -> a -> TestTree
testPropertyWithSeed name = singleTest name . QCWithSeed . QC . property

newtype QCWithSeed = QCWithSeed QC

instance IsTest QCWithSeed where
  testOptions = retag (testOptions :: Tagged QC [OptionDescription])

  run options (QCWithSeed test) progress = do
    replay <- case lookupOption options of
      QuickCheckReplayLegacy override -> return override
      _ -> getStdRandom random
    notice (mkVerbosity defaultVerbosityHandles normal) $ "Using --quickcheck-replay=" ++ show replay
    run (setOption (QuickCheckReplayLegacy replay) options) test progress

-- | Typeclass for doing arbitrary (but law-abiding) comparisons.  See also
-- 'ArbitraryOrd', this is the version that works with 'GHC.Generics'.
class GArbitraryOrd f where
  garbitraryCompare :: QC.Gen (f p -> f p -> Ordering)

instance GArbitraryOrd V1 where
  garbitraryCompare = pure $ \_ _ -> EQ

instance GArbitraryOrd U1 where
  garbitraryCompare = pure $ \_ _ -> EQ

instance (GArbitraryOrd f, GArbitraryOrd g) => GArbitraryOrd (f :+: g) where
  garbitraryCompare = do
    bias <- QC.arbitrary
    lcompare <- garbitraryCompare
    rcompare <- garbitraryCompare
    pure $ \l r ->
      let args = if bias then (l, r) else (r, l)
       in case args of
            (L1 x, L1 y) -> lcompare x y
            (L1 _, R1 _) -> LT
            (R1 x, R1 y) -> rcompare x y
            (R1 _, L1 _) -> GT

instance (GArbitraryOrd f, GArbitraryOrd g) => GArbitraryOrd (f :*: g) where
  garbitraryCompare = do
    bias <- QC.arbitrary
    xcompare <- garbitraryCompare
    ycompare <- garbitraryCompare
    pure $ \l r ->
      let (x1 :*: y1, x2 :*: y2) = if bias then (l, r) else (r, l)
       in case xcompare x1 x2 of
            LT -> LT
            EQ -> ycompare y1 y2
            GT -> GT

instance GArbitraryOrd f => GArbitraryOrd (M1 i t f) where
  garbitraryCompare = (\c (M1 l) (M1 r) -> c l r) <$> garbitraryCompare

instance ArbitraryOrd c => GArbitraryOrd (K1 i c) where
  garbitraryCompare = (\c (K1 l) (K1 r) -> c l r) <$> arbitraryCompare

-- | Typeclass for doing arbitrary (but law-abiding) comparisons.
class ArbitraryOrd a where
  arbitraryCompare :: QC.Gen (a -> a -> Ordering)
  default arbitraryCompare
    :: (Generic a, GArbitraryOrd (Rep a)) => QC.Gen (a -> a -> Ordering)
  arbitraryCompare = (\c l r -> c (from l) (from r)) <$> garbitraryCompare

instance ArbitraryOrd Char where
  arbitraryCompare = arbitraryCompareReverseSection

-- | Construct an arbitrary comparison by (conceptually) laying out all values
-- in a list, picking two values (since we are using arbitrary these should
-- be "good" values), and then reversing the section between these two values.
arbitraryCompareReverseSection
  :: (QC.Arbitrary a, Ord a) => QC.Gen (a -> a -> Ordering)
arbitraryCompareReverseSection = do
  x <- QC.arbitrary
  y <- QC.arbitrary
  let inside n = n >= min x y && n <= max x y
  pure $ \l r -> if inside l && inside r then compare r l else compare l r

instance ArbitraryOrd a => ArbitraryOrd [a] where
  arbitraryCompare = do
    shorterIsLess <- QC.arbitrary
    cmp <- arbitraryCompare
    let go [] [] = EQ
        go [] (_ : _) = if shorterIsLess then LT else GT
        go (_ : _) [] = if shorterIsLess then GT else LT
        go (x : xs) (y : ys) = case cmp x y of
          LT -> LT
          EQ -> go xs ys
          GT -> GT
    pure go

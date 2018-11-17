{-# LANGUAGE CPP #-}
module Distribution.Arbitrary.Util
  ( adjustSize
  , shortListOf
  , shortListOf1
  , ShortToken(..)
  , arbitraryShortToken
  , NonMEmpty (..)
  , arbitraryFlag
  )
  where

import Data.List
  ( isPrefixOf
  )
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
  ( Monoid (mempty)
  )
#endif
import Distribution.Simple.Flag
  ( Flag (..)
  )
import Test.QuickCheck
  ( Arbitrary ( arbitrary, shrink )
  , Gen
  , choose
  , frequency
  , resize
  , sized
  , suchThat
  , vectorOf
  )

-- | Adjust the size of the generated value.
--
-- In general the size gets bigger and bigger linearly. For some types
-- it is not appropriate to generate ever bigger values but instead
-- to generate lots of intermediate sized values. You could do that using:
--
-- > adjustSize (\n -> min n 5)
--
-- Similarly, for some types the linear size growth may mean getting too big
-- too quickly relative to other values. So you may want to adjust how
-- quickly the size grows. For example dividing by a constant, or even
-- something like the integer square root or log.
--
-- > adjustSize (\n -> n `div` 2)
--
-- Putting this together we can make for example a relatively short list:
--
-- > adjustSize (\n -> min 5 (n `div` 3)) (listOf1 arbitrary)
--
-- Not only do we put a limit on the length but we also scale the growth to
-- prevent it from hitting the maximum size quite so early.
--
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
    fmap ShortToken
      (shortListOf1 5 (choose ('#', '~'))
       `suchThat` (not . ("[]" `isPrefixOf`)))
    --TODO: [code cleanup] need to replace parseHaskellString impl to stop
    -- accepting Haskell list syntax [], ['a'] etc, just allow String syntax.
    -- Workaround, don't generate [] as this does not round trip.


  shrink (ShortToken cs) =
    [ ShortToken cs' | cs' <- shrink cs, not (null cs') ]

arbitraryShortToken :: Gen String
arbitraryShortToken = fmap getShortToken arbitrary

newtype NonMEmpty a = NonMEmpty { getNonMEmpty :: a }
  deriving (Eq, Ord, Show)

instance (Arbitrary a, Monoid a, Eq a) => Arbitrary (NonMEmpty a) where
  arbitrary = fmap NonMEmpty (arbitrary `suchThat` (/= mempty))
  shrink (NonMEmpty x) = [ NonMEmpty x' | x' <- shrink x, x' /= mempty ]

arbitraryFlag :: Gen a -> Gen (Flag a)
arbitraryFlag genA =
    sized $ \sz ->
      case sz of
        0 -> return NoFlag
        _ -> frequency [ (1, return NoFlag)
                       , (3, fmap Flag genA) ]

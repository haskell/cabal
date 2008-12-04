{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Distribution.Version where

import Distribution.Version

import Test.QuickCheck
import Test.QuickCheck.Utils

import Control.Monad (liftM, liftM2)


instance Arbitrary Version where
  arbitrary = do
    branch <- smallListOf1 $
                frequency [(3, return 0)
                          ,(3, return 1)
                          ,(2, return 2)
                          ,(1, return 3)]
    return (Version branch []) -- deliberate []
    where
      smallListOf1 = adjustSize (\n -> min 5 (n `div` 3)) . listOf1

  shrink (Version branch []) =
    [ Version branch' [] | branch' <- shrink branch ]
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
        , (1, liftM earlierVersion arbitrary)
        , (1, liftM orEarlierVersion arbitrary)
        , (1, liftM WildcardVersion arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 unionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 intersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

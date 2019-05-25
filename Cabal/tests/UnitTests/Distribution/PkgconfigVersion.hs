{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans
                -fno-warn-deprecations
                -fno-warn-incomplete-patterns #-}
module UnitTests.Distribution.PkgconfigVersion (pkgconfigVersionTests) where

import Distribution.Compat.Prelude.Internal
import Prelude ()

import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8 as BS8

import Distribution.Parsec                      (eitherParsec)
import Distribution.Pretty
import Distribution.Types.PkgconfigVersion
import Distribution.Types.PkgconfigVersionRange

pkgconfigVersionTests :: [TestTree]
pkgconfigVersionTests =
    [ testProperty "simpleParsec . prettyShow = Just" prop_parse_disp
    ]

prop_parse_disp :: PkgconfigVersionRange -> Property
prop_parse_disp vr = counterexample (show (prettyShow vr)) $
    eitherParsec (prettyShow vr) === Right vr

-------------------------------------------------------------------------------
-- Arbitrary instances
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
  arbitrary = sized verRangeExp
    where
      verRangeExp n = frequency $
        [ (2, return PcAnyVersion)
        , (1, liftM PcThisVersion arbitrary)
        , (1, liftM PcLaterVersion arbitrary)
        , (1, liftM PcOrLaterVersion arbitrary)
        , (1, liftM orLaterVersion' arbitrary)
        , (1, liftM PcEarlierVersion arbitrary)
        , (1, liftM PcOrEarlierVersion arbitrary)
        , (1, liftM orEarlierVersion' arbitrary)
        ] ++ if n == 0 then [] else
        [ (2, liftM2 PcUnionVersionRanges     verRangeExp2 verRangeExp2)
        , (2, liftM2 PcIntersectVersionRanges verRangeExp2 verRangeExp2)
        ]
        where
          verRangeExp2 = verRangeExp (n `div` 2)

      orLaterVersion'   v =
        PcUnionVersionRanges (PcLaterVersion v)   (PcThisVersion v)
      orEarlierVersion' v =
        PcUnionVersionRanges (PcEarlierVersion v) (PcThisVersion v)

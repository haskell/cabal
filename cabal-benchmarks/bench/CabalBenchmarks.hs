{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Main where

import Test.Tasty.Bench                       (bench, bgroup, defaultMain, env, nf, whnf)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Parsec                    (eitherParsec)
import Distribution.Version

import qualified Data.ByteString as BS

import qualified Distribution.Types.VersionInterval.Legacy as Old
import qualified Distribution.Types.VersionInterval        as New

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ bgroup "parseGPD"
        [ env (BS.readFile "Cabal/Cabal.cabal") $ \bs ->
          bench "Cabal" $ whnf parseGenericPackageDescriptionMaybe bs
        , env (BS.readFile "cabal-benchmarks/cabal-benchmarks.cabal") $ \bs ->
          bench "cabal-benchmarks" $ whnf parseGenericPackageDescriptionMaybe bs
        ]

    , bgroup "normaliseVersionRange" $
        let suite name f = bgroup name
                [ env bigVersionRange1 $ \vr -> bench "dnf1" $ nf f vr
                , env bigVersionRange2 $ \vr -> bench "dnf2" $ nf f vr
                , env bigVersionRange3 $ \vr -> bench "cnf1" $ nf f vr
                , env bigVersionRange4 $ \vr -> bench "cnf2" $ nf f vr
                , env bigVersionRange5 $ \vr -> bench "mix1" $ nf f vr
                , env bigVersionRange6 $ \vr -> bench "mix2" $ nf f vr
                , env bigVersionRange7 $ \vr -> bench "pat1" $ nf f vr
                , env bigVersionRange8 $ \vr -> bench "pat2" $ nf f vr
                , env bigVersionRange9 $ \vr -> bench "pat3" $ nf f vr
                , env bigVersionRangeA $ \vr -> bench "pat4" $ nf f vr
                ]
        in  [ suite "def" normaliseVersionRange
            , suite "old" oldNormaliseVersionRange
            , suite "new" newNormaliseVersionRange
            ]
    ]

-------------------------------------------------------------------------------
-- VersionRanges normalisation
-------------------------------------------------------------------------------

oldNormaliseVersionRange :: VersionRange -> VersionRange
oldNormaliseVersionRange = Old.fromVersionIntervals . Old.toVersionIntervals

newNormaliseVersionRange :: VersionRange -> VersionRange
newNormaliseVersionRange = New.normaliseVersionRange2

bigVersionRange1 :: IO VersionRange
bigVersionRange1 = either fail return $ eitherParsec
    "(>=1.2.0 && <1.3) || (>=1.3.0 && <1.4) || (>=1.4.0.0 && <1.5) || (>=1.5.0.0 && <1.6) || (>=1.7.0.0 && <1.8)"

bigVersionRange2 :: IO VersionRange
bigVersionRange2 = either fail return $ eitherParsec
    "(>=1.2.0 && <1.3) || (>=1.4.0.0 && <1.5) || (>=1.3.0 && <1.4) || (>=1.5.0.0 && <1.6) || (>=1.7.0.0 && <1.8)"

bigVersionRange3 :: IO VersionRange
bigVersionRange3 = either fail return $ eitherParsec
    ">=1.2.0 && (<1.3 || >=1.3.0) && (<1.4 || >=1.4.0.0) && (<1.5 || >=1.5.0.0) && (<1.6 || >=1.7.0.0) && <1.8"

bigVersionRange4 :: IO VersionRange
bigVersionRange4 = either fail return $ eitherParsec
    ">=1.2.0 && <1.8 && (<1.4 || >=1.4.0.0) && (<1.3 || >=1.3.0) && (<1.5 || >=1.5.0.0) || (<1.6 && >=1.7.0.0)"

bigVersionRange5 :: IO VersionRange
bigVersionRange5 = either fail return $ eitherParsec
    ">=1.2.0 && (<1.3 || >=1.3.0) && (<1.4 || (>=1.4.0.0 && <1.5) || >=1.5.0.0) && (<1.6 || (>=1.7.0.0 && (<1.8 || >=1.9) && <1.10) || >=1.11) && <1.12"

bigVersionRange6 :: IO VersionRange
bigVersionRange6 = fmap New.normaliseVersionRange2 bigVersionRange5

bigVersionRange7 :: IO VersionRange
bigVersionRange7 = return $
    i2 $ i2 $ u (b 0 1) (b 0 1)
  where
    i2 x = i x x
    i = intersectVersionRanges
    u = unionVersionRanges
    b x y = intersectVersionRanges (laterVersion (v x)) (earlierVersion (v y))
    v x = mkVersion [x]

bigVersionRange8 :: IO VersionRange
bigVersionRange8 = return $
    i2 $ i2 $ i2 $ u (b 0 1) (b 0 1)
  where
    i2 x = i x x
    i = intersectVersionRanges
    u = unionVersionRanges
    b x y = intersectVersionRanges (laterVersion (v x)) (earlierVersion (v y))
    v x = mkVersion [x]

bigVersionRange9 :: IO VersionRange
bigVersionRange9 = return $
    i2 $ i2 $ i2 $ i2 $ u (b 0 1) (b 0 1)
  where
    i2 x = i x x
    i = intersectVersionRanges
    u = unionVersionRanges
    b x y = intersectVersionRanges (laterVersion (v x)) (earlierVersion (v y))
    v x = mkVersion [x]

bigVersionRangeA :: IO VersionRange
bigVersionRangeA = return $
    i2 $ i2 $ i2 $ i2 $ i2 $ u (b 0 1) (b 0 1)
  where
    i2 x = i x x
    i = intersectVersionRanges
    u = unionVersionRanges
    b x y = intersectVersionRanges (laterVersion (v x)) (earlierVersion (v y))
    v x = mkVersion [x]

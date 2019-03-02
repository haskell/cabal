{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module Main where

import Data.Monoid      ((<>))
import Data.Word        (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types  (CInt (..))
import System.IO.Unsafe (unsafePerformIO)

import Test.QuickCheck       (Arbitrary (..), (===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.HUnit      (assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Distribution.Pretty        (prettyShow)
import Distribution.Types.Version

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

-------------------------------------------------------------------------------
-- C reference implementation
-------------------------------------------------------------------------------

foreign import ccall unsafe "rpmvercmp" c_rmpvercmp
    :: CString -> CString -> CInt

rpmvercmpRef :: BS.ByteString -> BS.ByteString -> Ordering
rpmvercmpRef a b = unsafePerformIO $
    BS.useAsCString a $ \a' ->
    BS.useAsCString b $ \b' ->
        return $ fromInt $ c_rmpvercmp a' b'
  where
    fromInt = flip compare 0

-------------------------------------------------------------------------------
-- Pure implementation
-------------------------------------------------------------------------------

rpmvercmpPure :: BS.ByteString -> BS.ByteString -> Ordering
rpmvercmpPure a b = go0 (BS.unpack a) (BS.unpack b)
  where
    go0 :: [Word8] -> [Word8] -> Ordering
    go0 xs ys = go1 (dropNonAlnum8 xs) (dropNonAlnum8 ys)

    go1 :: [Word8] -> [Word8] -> Ordering
    go1 [] [] = EQ
    go1 [] _  = LT
    go1 _  [] = GT
    go1 xs@(x:_) ys
      | isDigit8 x =
          let (xs1, xs2) = span isDigit8 xs
              (ys1, ys2) = span isDigit8 ys
            -- numeric segments are always newer than alpha segments
          in if null ys1
             then GT
             else compareInt xs1 ys1 <> go0 xs2 ys2

      -- isAlpha
      | otherwise =
          let (xs1, xs2) = span isAlpha8 xs
              (ys1, ys2) = span isAlpha8 ys
          in if null ys1
             then LT
             else compareStr xs1 ys1 <> go0 xs2 ys2

-- compare as numbers
compareInt :: [Word8] -> [Word8] -> Ordering
compareInt xs ys =
    -- whichever number has more digits wins
    compare (length xs') (length ys') <>
    -- equal length: use per character compare, "strcmp"
    compare xs' ys'
  where
    -- drop  leading zeros
    xs' = dropWhile (== 0x30) xs
    ys' = dropWhile (== 0x30) ys

-- strcmp
compareStr :: [Word8] -> [Word8] -> Ordering
compareStr = compare

dropNonAlnum8 :: [Word8] -> [Word8]
dropNonAlnum8 = dropWhile (\w -> not (isDigit8 w || isAlpha8 w))

isDigit8 :: Word8 -> Bool
isDigit8 w = 0x30 <= w && w <= 0x39

isAlpha8 :: Word8 -> Bool
isAlpha8 w = (0x41 <= w && w <= 0x5A) || (0x61 <= w && w <= 0x7A)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "rpmvercmp"
    [ testGroup "examples"
        [ example "openssl" "1.1.0g" "1.1.0i" LT
        , example "openssl" "1.0.2h" "1.1.0"  LT

        , example "simple" "1.2.3" "1.2.4" LT
        , example "word" "apple" "banana" LT

        , example "corner case" "r" "" GT
        , example "corner case" "0" "1" LT
        , example "corner case" "1" "0.0" GT
        ]
    , testGroup "Properties"
        [ testProperty "ref reflexive" $ \a ->
            rpmvercmpRef (BS.pack a) (BS.pack a) === EQ
        , testProperty "pure reflexive" $ \a ->
            rpmvercmpPure (BS.pack a) (BS.pack a) === EQ
        , testProperty "ref agrees with Version" $ \a b ->
            compare a b === rpmvercmpRef (v2bs a) (v2bs b)
        , testProperty "pure agrees with Version" $ \a b ->
            compare a b === rpmvercmpPure (v2bs a) (v2bs b)
        ]
    , testGroup "Random inputs"
        [ testProperty "random" $ \xs ys ->
            let xs' = BS.pack $ unnull $ filter (/= 0) xs
                ys' = BS.pack $ unnull $ filter (/= 0) ys

                -- ref doesn't really work with empty inputs reliably.
                unnull [] = [1]
                unnull zs = zs
            in rpmvercmpRef xs' ys' === rpmvercmpPure xs' ys'
        ]
    ]
  where
    example n a b c = testCase (n ++ " " ++ BS8.unpack a ++ " <=> " ++ BS8.unpack b) $ do
        let ref = rpmvercmpRef a b
        let pur = rpmvercmpPure a b
        assertEqual "ref" c ref
        assertEqual "pure" c pur

-------------------------------------------------------------------------------
-- Version arbitrary
-------------------------------------------------------------------------------

newtype V = V Version
  deriving (Show, Eq, Ord)

unV :: V -> Version
unV (V x) = x

instance Arbitrary V where
    arbitrary = V . mkVersion_ <$> arbitrary

    shrink = map V . filter (/= version0) . map mkVersion_ . shrink . versionNumbers . unV

mkVersion_ :: [Int] -> Version
mkVersion_ [] = version0
mkVersion_ xs = mkVersion (map abs xs)

v2bs :: V -> BS.ByteString
v2bs (V x) = BS8.pack (prettyShow x)

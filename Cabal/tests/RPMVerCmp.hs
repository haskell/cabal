{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
module Main where

import Foreign.C.String (CString)
import Foreign.C.Types  (CInt (..))
import System.IO.Unsafe (unsafePerformIO)
import Data.Bits ((.&.))

import Test.QuickCheck       (Arbitrary (..), (===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.HUnit      (assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

import Distribution.Pretty                 (prettyShow)
import Distribution.Types.PkgconfigVersion (rpmvercmp)
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
            rpmvercmp (BS.pack a) (BS.pack a) === EQ
        , testProperty "ref agrees with Version" $ \a b ->
            compare a b === rpmvercmpRef (v2bs a) (v2bs b)
        , testProperty "pure agrees with Version" $ \a b ->
            compare a b === rpmvercmp (v2bs a) (v2bs b)
        ]
    , testGroup "Random inputs"
        [ testProperty "random" $ \xs ys ->
            -- only 7bit numbers, no zero, and non-empty.
            let xs' = BS.pack $ unnull $ filter (/= 0) $ map (.&. 0x7f) xs
                ys' = BS.pack $ unnull $ filter (/= 0) $ map (.&. 0x7f) ys

                -- ref doesn't really work with empty inputs reliably.
                unnull [] = [1]
                unnull zs = zs
            in rpmvercmpRef xs' ys' === rpmvercmp xs' ys'
        ]
    ]
  where
    example n a b c = testCase (n ++ " " ++ BS8.unpack a ++ " <=> " ++ BS8.unpack b) $ do
        let ref = rpmvercmpRef a b
        let pur = rpmvercmp a b
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
    arbitrary = fmap (V . mkVersion_) arbitrary

    shrink = map V . filter (/= version0) . map mkVersion_ . shrink . versionNumbers . unV

mkVersion_ :: [Int] -> Version
mkVersion_ [] = version0
mkVersion_ xs = mkVersion (map abs xs)

v2bs :: V -> BS.ByteString
v2bs (V x) = BS8.pack (prettyShow x)

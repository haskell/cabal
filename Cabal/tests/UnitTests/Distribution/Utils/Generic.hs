{-# LANGUAGE OverloadedStrings #-}

-- to suppress WARNING in "Distribution.Compat.Prelude.Internal"
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module UnitTests.Distribution.Utils.Generic ( tests ) where

import Prelude ()
import Distribution.Compat.Prelude.Internal

import Distribution.Utils.Generic

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
    [ -- fromUTF8BS / toUTF8BS
      testCase "fromUTF8BS mempty" testFromUTF8BSEmpty
    , testCase "toUTF8BS mempty" testToUTF8BSEmpty
    , testCase "toUTF8BS [U+D800..U+DFFF]" testToUTF8BSSurr
    , testCase "toUTF8BS [U+0000..U+7F]"   testToUTF8BSAscii
    , testCase "toUTF8BS [U+0000..U+10FFFF]" testToUTF8BSText
    , testCase "fromUTF8BS.toUTF8BS [U+0000..U+10FFFF]" testToFromUTF8BS

    , testProperty "fromUTF8BS.toUTF8BS == id" prop_toFromUTF8BS
    , testProperty "toUTF8BS == encodeUtf8" prop_toUTF8BS
    ]

testFromUTF8BSEmpty :: Assertion
testFromUTF8BSEmpty = mempty @=? fromUTF8BS mempty

testToUTF8BSEmpty :: Assertion
testToUTF8BSEmpty = mempty @=? toUTF8BS mempty

testToUTF8BSSurr :: Assertion
testToUTF8BSSurr = BS.concat (replicate 2048 u_fffd) @=? toUTF8BS surrogates
  where
    surrogates = ['\xD800'..'\xDFFF']
    u_fffd = "\xEF\xBF\xBD"

testToUTF8BSText :: Assertion
testToUTF8BSText = T.encodeUtf8 (T.pack txt) @=? toUTF8BS txt
  where
    txt = ['\x00'..'\x10FFFF']

testToUTF8BSAscii :: Assertion
testToUTF8BSAscii = BS.pack txt @=? toUTF8BS txt
  where
    txt = ['\x00'..'\x7F']

testToFromUTF8BS :: Assertion
testToFromUTF8BS = txt @=? (fromUTF8BS . toUTF8BS) txt
  where
    txt = ['\x0000'..'\xD7FF'] ++ ['\xE000'..'\x10FFFF']

prop_toFromUTF8BS :: [Char] -> Property
prop_toFromUTF8BS txt = txt === (fromUTF8BS . toUTF8BS) txt

prop_toUTF8BS :: [Char] -> Property
prop_toUTF8BS txt = T.encodeUtf8 (T.pack txt) === toUTF8BS txt

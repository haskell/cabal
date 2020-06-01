module UnitTests.Distribution.Compat.CreatePipe (tests) where

import Control.Concurrent.Async (async, wait)
import Control.DeepSeq          (force)
import Control.Exception        (evaluate)
import System.IO                (hClose, hGetContents, hPutStr, hSetEncoding, localeEncoding)
import Test.Tasty               (TestTree)
import Test.Tasty.HUnit         (Assertion, assertEqual, testCase)

import qualified Data.ByteString as BS

import Distribution.Compat.CreatePipe

tests :: [TestTree]
tests =
    [ testCase "Locale Encoding" case_Locale_Encoding
    , testCase "Binary ByteStrings are not affected" case_ByteString
    ]

case_Locale_Encoding :: Assertion
case_Locale_Encoding = do
    let str = "\0252foobar"
    (r, w) <- createPipe
    hSetEncoding w localeEncoding
    hSetEncoding r localeEncoding

    ra <- async $ do
        out <- hGetContents r
        evaluate (force out)

    wa <- async $ do
        hPutStr w str
        hClose w

    out <- wait ra
    wait wa

    assertEqual "createPipe should support Unicode roundtripping" str out

case_ByteString :: Assertion
case_ByteString = do
    let bs = BS.pack[ 1..255]
    (r, w) <- createPipe

    ra <- async $ do
        out <- BS.hGetContents r
        evaluate (force out)

    wa <- async $ do
        BS.hPutStr w bs
        hClose w

    out <- wait ra
    wait wa

    assertEqual "createPipe should support Unicode roundtripping" bs out

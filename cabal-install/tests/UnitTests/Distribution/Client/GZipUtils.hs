module UnitTests.Distribution.Client.GZipUtils (
  tests
  ) where

import Distribution.Client.GZipUtils         (maybeDecompress)
import Data.ByteString.Lazy.Char8    as BS   (pack)
import Codec.Compression.Zlib        as Zlib
import Codec.Compression.GZip        as GZip

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = [ testCase "maybeDecompress" maybeDecompressUnitTest
        ]

maybeDecompressUnitTest :: Assertion
maybeDecompressUnitTest =
        assertBool "decompress plain" (maybeDecompress original       == original)
     >> assertBool "decompress zlib"  (maybeDecompress compressedZlib == original)
     >> assertBool "decompress gzip"  (maybeDecompress compressedGZip == original)
  where
    original = BS.pack "original uncompressed input"
    compressedZlib = Zlib.compress original
    compressedGZip = GZip.compress original

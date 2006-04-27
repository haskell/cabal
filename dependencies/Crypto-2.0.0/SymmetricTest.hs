module Main(main) where

import Codec.Utils
import Codec.Encryption.Blowfish as Blowfish
import Codec.Encryption.Modes
import Codec.Encryption.Padding
import Codec.Encryption.DES as DES
import Codec.Encryption.AES as AES
import Data.Word
import Data.Bits
import Data.Char
import Data.LargeWord
import Test.HUnit
import Numeric

-- AES Tests.

-- Comparision with resuts from 
-- http://www.cs.ucsd.edu/~fritz/rijndael_test.html

ad   = 0xF0E1D2C3B4A5968778695A4B3C2D1E0F :: Word128
ak16 = 0xF0E1D2C3B4A5968778695A4B3C2D1E0F :: Word128
ae16 = 0x6e94fffbb861b2c1769cd4629f3d724b :: Word128
ae16' = AES.encrypt ak16 ad
aesTest1 = 
   TestCase (
      assertEqual "AES Key Length 128" ae16 ae16'
   )

ak24 = 0xF0E1D2C3B4A5968778695A4B3C2D1E0FF0E1D2C3B4A59687 :: Word192
ae24' = AES.encrypt ak24 ad
ae24 = 0x07d806bb62ebb4399354594ea6586ec6 :: Word128
aesTest2 = 
   TestCase (
      assertEqual "AES Key Length 192" ae24 ae24'
   )

ak32 = 0xF0E1D2C3B4A5968778695A4B3C2D1E0FF0E1D2C3B4A5968778695A4B3C2D1E0F :: Word256
ae32' = AES.encrypt ak32 ad
ae32 = 0x28e88482d9b146fdde7e080fcbae1b98 :: Word128
aes3 = ae32 == ae32'
aesTest3 = 
   TestCase (
      assertEqual "AES Key Length 256" ae32 ae32'
   )

-- Comparision with resuts from 
-- http://www.zvon.org/tmRFC/RFC3602/Output/chapter4.html

aeskey16 = 0x06a9214036b8a15b512e03d534120006 :: Word128
aesiv16  = 0x3dafba429d9eb430b422da802c9fac41 :: Word128
aesplaintext = "Single block msg"
aesciphertext = [0xe353779c1079aeb82708942dbe77181a] :: [Word128]
aesciphertext' =
   cbc AES.encrypt aesiv16 aeskey16 $ 
   pkcs5 $ 
   map (fromIntegral . ord) aesplaintext
-- aescbc1 = aesciphertext == take 1 aesciphertext'
aescbcTest1 = 
   TestCase (
      assertEqual "AES CBC PKCS5 Key Length 128" 
                  aesciphertext (take 1 aesciphertext')
   )

aesplaintext' =
   map (chr . fromIntegral) $ 
   unPkcs5 $ 
   unCbc AES.decrypt aesiv16 aeskey16 aesciphertext'
aescbcTest2 = 
   TestCase (
      assertEqual "AES CBC PKCS5 Decryption Key Length 128" 
                  aesplaintext aesplaintext'
   )

aes2key16 = 0x6c3ea0477630ce21a2ce334aa746c2cd :: Word128
aes2iv16  = 0xc782dc4c098c66cbd9cd27d825682c81 :: Word128
aes2plaintext = "This is a 48-byte message (exactly 3 AES blocks)"
aes2ciphertext = [0xd0a02b3836451753d493665d33f0e886,
                  0x2dea54cdb293abc7506939276772f8d5,
                  0x021c19216bad525c8579695d83ba2684] :: [Word128]
aes2ciphertext' =
   cbc AES.encrypt aes2iv16 aes2key16 $ 
   pkcs5 $ 
   map (fromIntegral . ord) aes2plaintext
aescbcTest3 = 
   TestCase (
      assertEqual "AES CBC PKCS5 3 Blocks Key Length 128" 
                  aes2ciphertext (take 3 aes2ciphertext')
   )

-- Comparision with resuts from 
-- http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf

aes3key24 = 
   0x8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b :: Word192
aes3iv16 = 0x000102030405060708090a0b0c0d0e0f :: Word128
aes3plaintext = 
   [0x6bc1bee22e409f96e93d7e117393172a,
    0xae2d8a571e03ac9c9eb76fac45af8e51,
    0x30c81c46a35ce411e5fbc1191a0a52ef,
    0xf69f2445df4f9b17ad2b417be66c3710] :: [Word128]
aes3ciphertext' =
   cbc AES.encrypt aes3iv16 aes3key24 $ 
   pkcs5 $ 
   concat $ 
   map (toOctets 256) aes3plaintext
aes3ciphertext =
   [0x4f021db243bc633d7178183a9fa071e8,
    0xb4d9ada9ad7dedf4e5e738763f69145a,
    0x571b242012fb7ae07fa9baac3df102e0,
    0x08b0e27988598881d920a9e64f5615cd] :: [Word128]
aescbcTest4 = 
   TestCase (
      assertEqual "AES CBC PKCS5 4 Blocks Key Length 192" 
                  aes3ciphertext (take 4 aes3ciphertext')
   )

aes4key32 = 
   0x603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4
   :: Word256
aes4iv16 = 0x000102030405060708090a0b0c0d0e0f :: Word128
aes4plaintext =
   [0x6bc1bee22e409f96e93d7e117393172a,
    0xae2d8a571e03ac9c9eb76fac45af8e51,
    0x30c81c46a35ce411e5fbc1191a0a52ef,
    0xf69f2445df4f9b17ad2b417be66c3710] :: [Word128]
aes4ciphertext' =
   cbc AES.encrypt aes4iv16 aes4key32 $ 
   pkcs5 $ 
   concat $ 
   map (toOctets 256) aes3plaintext
aes4ciphertext = 
   [0xf58c4c04d6e5f1ba779eabfb5f7bfbd6,
    0x9cfc4e967edb808d679f777bc6702c7d,
    0x39f23369a9d9bacfa530e26304231461,
    0xb2eb05e2c39be9fcda6c19078c6a9d1b] :: [Word128]
aescbcTest5 = 
   TestCase (
      assertEqual "AES CBC PKCS5 4 Blocks Key Length 256" 
                  aes4ciphertext (take 4 aes4ciphertext')
   )

-- Blowfish Tests.

-- Tests from http://www.counterpane.com/vectors.txt.

d = 0xFEDCBA9876543210 :: Word64
k = 0xF0 :: Word8
e = 0xF9AD597C49DB005E :: Word64
e' = Blowfish.encrypt k d
bfTest1 = 
   TestCase (
      assertEqual "Blowfish Key Length 8" e e'
   )

k2 = 0xF0E1 :: Word16
e2 = 0xE91D21C1D961A6D6 :: Word64
e2' = Blowfish.encrypt k2 d
bfTest2 = 
   TestCase (
      assertEqual "Blowfish Key Length 16" e2 e2'
   )

e8 = 0xE87A244E2CC85E82 :: Word64
k8 = 0xF0E1D2C3B4A59687 :: Word64
e8' = Blowfish.encrypt k8 d
bfTest3 = 
   TestCase (
      assertEqual "Blowfish Key Length 64" e8 e8'
   )

e16 = 0x93142887EE3BE15C :: Word64
k16= 0xF0E1D2C3B4A5968778695A4B3C2D1E0F :: Word128
e16' = Blowfish.encrypt k16 d
bfTest4 = 
   TestCase (
      assertEqual "Blowfish Key Length 128" e16 e16'
   )

-- Blowfish test with Cipher Block Chaining.
-- Set up the published key, initialization vector and data.

key16 = 0x0123456789ABCDEFF0E1D2C3B4A59687 :: Word128
iv8     = 0xFEDCBA9876543210 :: Word64
data29  = "7654321 Now is the time for \NUL"

-- Pad with nulls as in the example.

e29' = 
   cbc Blowfish.encrypt iv8 key16 $ padNulls $ map (fromIntegral . ord) data29

e29 = [0x6B,0x77,0xB4,0xD6,0x30,0x06,0xDE,0xE6,
       0x05,0xB1,0x56,0xE2,0x74,0x03,0x97,0x93,
       0x58,0xDE,0xB9,0xE7,0x15,0x46,0x16,0xD9,
       0x59,0xF1,0x65,0x2B,0xD5,0xFF,0x92,0xCC] :: [Octet]

bfTest5 = 
   TestCase (
      assertEqual "AES CBC Nulls Key Length 128" 
                  e29 (concat $ map (toOctets 256) e29')
   )

-- DES Tests

-- Test from http://www.itl.nist.gov/fipspubs/fip81.htm

key = 0x0123456789abcdef :: Word64
iv = 0x1234567890abcdef :: Word64
expectedDES = [0xe5c7cdde872bf27c,
               0x43e934008c389c0f,
               0x683788499a7c05f6] :: [Word64]
plainText = "Now is the time for all " 

-- Pad using PKCS#5 so only take the first 3 blocks of the ciphertext.

cipherText = 
   cbc DES.encrypt iv key $ pkcs5 $ map (fromIntegral . ord) plainText
descbcTest1 = 
   TestCase (
      assertEqual "DES CBC PKCS5 3 Blocks" 
                  expectedDES (take 3 cipherText)
   )

tests = 
   TestList [
      aesTest1, aesTest2, aesTest3, aescbcTest1,
      aescbcTest2, aescbcTest3, aescbcTest4, aescbcTest5,
      bfTest1, bfTest2, bfTest3, bfTest4, bfTest5,
      descbcTest1
   ]

main = runTestTT tests

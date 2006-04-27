module Main where

import Codec.Utils
import Codec.Encryption.Blowfish as Blowfish
import Codec.Encryption.AES as AES
import Codec.Encryption.Modes
import Codec.Encryption.Padding
import Codec.Binary.Base64
import Data.LargeWord
import Data.Word
import Data.Bits
import Numeric
import Data.Char
import Test.QuickCheck

instance Arbitrary Word8 where
   arbitrary = 
      do n <- choose ((fromIntegral (minBound::Word8))::Int, 
                      (fromIntegral (maxBound::Word8))::Int)
         return (fromIntegral n)

instance Arbitrary Word64 where
   arbitrary = 
      do n <- choose ((fromIntegral (minBound::Word64))::Integer, 
                      (fromIntegral (maxBound::Word64))::Integer)
         return (fromIntegral n)

instance Arbitrary Word128 where
   arbitrary = 
      do n <- choose ((fromIntegral (minBound::Word128))::Integer, 
                      (fromIntegral (maxBound::Word128))::Integer)
         return (fromIntegral n)

prop_decryptEncrypt k b = b == Blowfish.decrypt k (Blowfish.encrypt k b)
   where types = (k :: Word8, b :: Word64)

prop_AESIdempotent k b = b == AES.decrypt k (AES.encrypt k b)
   where types = (k :: Word128, b :: Word128)

prop_unCbcCbc iv k bs =
   bs == (unPkcs5 $ unCbc Blowfish.decrypt iv k $ cbc Blowfish.encrypt iv k $ pkcs5 bs)
      where types =(k :: Word8, iv :: Word64, bs :: [Octet])

prop_unPkcs5Pkcs5 os =
   os == (unPkcs5 $ ((pkcs5 os)::[Word64]))
      where types = (os :: [Octet])

prop_unNullsNulls os =
   all (/=0) os ==>
      os == (unPadNulls $ ((padNulls os)::[Word64]))
         where types = (os :: [Octet])

prop_fromOctetsToOctets k n =
   k >= 0 && n > 1 ==>
      k == (fromOctets n $ toOctets n k)
         where types = (k :: Int, n :: Word8)

prop_unTwosCompTwosComp n =
   n < (0) ==>
      collect n $ 
      forAll g $ 
      \n -> n == (fromTwosComp $ toTwosComp n)
      where types = (n::Int)
            g = do n <- choose (minBound::Int,maxBound::Int)
                   return (13*n)

prop_base64 os =
   os == g os
      where types = (os :: [Word8])
            g = decode . encode

prop_chop72 os =
   os == g os
      where types = (os :: [Word8])
            g = decode . chop72 . encode

main = do quickCheck prop_base64
          quickCheck prop_chop72
          quickCheck prop_decryptEncrypt
          quickCheck prop_AESIdempotent
          quickCheck prop_unCbcCbc 
          quickCheck prop_unPkcs5Pkcs5
          quickCheck prop_unNullsNulls
          quickCheck prop_fromOctetsToOctets
          quickCheck prop_unTwosCompTwosComp
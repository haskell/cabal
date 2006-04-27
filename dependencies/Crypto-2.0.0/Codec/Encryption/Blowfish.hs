-----------------------------------------------------------------------------
-- |
-- Module      :  Coded.Encryption.Blowfish
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Takes the Blowfish module supplied by Doug Hoyte and wraps it so it can
-- used with the standard modes.
--
-----------------------------------------------------------------------------

module Codec.Encryption.Blowfish
   (
   -- * Function Types
   encrypt,
   decrypt
   ) where

import Data.Bits
import Data.Word
import Data.Char
import Codec.Utils
import Codec.Encryption.BlowfishAux

-- * Basic Blowfish Encryption

-- | Basic Blowfish encryption which takes a key and a block of plaintext 
-- and returns the encrypted block of ciphertext according to the standard.
-- Typical keys are Word8, Word16, Word32, Word64, Word128. See 
-- <http://www.counterpane.com/vectors.txt>.

encrypt :: (Integral a) => a -> Word64 -> Word64
encrypt k p = mergeWord32 (lo,hi) where
   lo = head e
   hi = head $ tail e
   e = bfEnc (bfMakeKey (map (chr . fromIntegral) (toOctets 256 k))) [lo',hi']
   (lo',hi') = (splitZord64 p)

-- | Basic Blowfish decryption which takes a key and a block of ciphertext
-- and returns the decrypted block of plaintext.

decrypt :: (Integral a) => a -> Word64 -> Word64
decrypt k p = mergeWord32 (lo,hi) where
   lo = head d
   hi = head $ tail d
   d = bfDec (bfMakeKey (map (chr . fromIntegral) (toOctets 256 k))) [lo',hi']
   (lo',hi') = splitZord64 p

splitZord64 :: Word64 -> (Word32,Word32)
splitZord64 x = (fromIntegral (shiftR (x .&. 0xffffffff00000000) 32),
                 fromIntegral (x .&. 0x00000000ffffffff)) 

mergeWord32 :: (Word32,Word32) -> Word64
mergeWord32 (lo,hi) = shift (fromIntegral lo) 32 + fromIntegral hi

-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.AES
-- Copyright   :  (c) Dominic Steinitz 2004
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Takes the AES module supplied by Lukasz Anforowicz and wraps it so it can
-- used with the standard modes.
--
-----------------------------------------------------------------------------

module Codec.Encryption.AES (
   -- * Function Types 
   encrypt, decrypt) where

import Codec.Encryption.AESAux
import Data.LargeWord
import Codec.Utils
import Data.Word
import Data.Bits

class (Bits a, Integral a) => AESKey a

instance AESKey Word128
instance AESKey Word192
instance AESKey Word256

-- | Basic AES encryption which takes a key and a block of plaintext 
-- and returns the encrypted block of ciphertext according to the standard.

encrypt :: AESKey a => a -> Word128 -> Word128
encrypt k p = 
   case bitSize k of
      128 -> f aes128Encrypt k p
      192 -> f aes192Encrypt k p
      256 -> f aes256Encrypt k p

f g k p = 
   fromIntegral $ fromOctets 256 $ 
      g (i2osp (bitSize k `div` bitSize (0::Octet)) $ fromIntegral k) 
        (i2osp (bitSize p `div` bitSize (0::Octet)) $ fromIntegral p)

-- | Basic AES decryption which takes a key and a block of ciphertext and
-- returns the decrypted block of plaintext according to the standard.

decrypt :: AESKey a => a -> Word128 -> Word128
decrypt k p = 
   case bitSize k of
      128 -> f aes128Decrypt k p
      192 -> f aes192Decrypt k p
      256 -> f aes256Decrypt k p


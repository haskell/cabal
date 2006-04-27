-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.DES
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Takes the DES module supplied by Ian Lynagh and wraps it so it can
-- used with the standard modes.
--
-- See <http://web.comlab.ox.ac.uk/oucl/work/ian.lynagh/>.
--
-----------------------------------------------------------------------------

module Codec.Encryption.DES (
   -- * Function Types 
   encrypt, decrypt) where

import Codec.Encryption.DESAux
import Word

-- | Basic DES encryption which takes a key and a block of plaintext 
-- and returns the encrypted block of ciphertext according to the standard.

encrypt :: Word64 -> Word64 -> Word64
encrypt = flip des_enc

-- | Basic DES decryption which takes a key and a block of ciphertext and
-- returns the decrypted block of plaintext according to the standard.

decrypt :: Word64 -> Word64 -> Word64
decrypt = flip des_dec

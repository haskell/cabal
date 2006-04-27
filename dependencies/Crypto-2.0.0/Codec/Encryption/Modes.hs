-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.Modes
-- Copyright   :  (c) Dominic Steinitz 2001-2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- This module currently supports Cipher Block Chaining (CBC) mode.
-- See <http://www.itl.nist.gov/fipspubs/fip81.htm> for further details.
-- 
-----------------------------------------------------------------------------

module Codec.Encryption.Modes (
   -- * Function types
   cbc, unCbc 
	      ) where

import Data.Word
import Data.Bits

-- * CBC or Cipher Block Chaining Mode

-- | In CBC or Cipher Block Chaining mode each block is XORed with 
-- the previous enciphered block before encryption.  For the first 
-- block, start with an initialization vector.
-- Take an encryption function, an initialisation vector, a key and
-- a list of blocks and return the encrypted blocks using CBC.

cbc :: Bits block =>
       (key -> block -> block) -> 
       block -> 
       key ->
       [block] -> 
       [block]

cbc e iv k ps = 
   ciphers where
      ciphers = map (e k) feedIns
      feedIns = zipWith xor (iv : ciphers) ps

-- | To  decipher in CBC or Cipher Block Chaining mode, decipher 
-- each block, then XOR the result with the previous block of 
-- plaintext result.  Note that the initialization vector is treated as the 
-- zeroth block of plaintext.
-- Take a decryption function, an initialisation vector, a key and a list
-- of encrypted blocks using CBC and return plaintext blocks.

unCbc :: Bits block =>
         (key -> block -> block) -> 
         block -> 
         key ->
         [block] -> 
         [block]

unCbc d iv k ms =
   outOfCbcs where
      beforeXOrs = map (d k) ms
      outOfCbcs  = zipWith xor (iv : ms) beforeXOrs

-----------------------------------------------------------------------------
-- |
-- Module      :  Coded.Encryption.RSA
-- Copyright   :  (c) David J. Sankel 2003, Dominic Steinitz 2003
-- License     :  GPL (see the file ReadMe.tex)
--
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A modified version of the RSA module supplied by David J. Sankel
-- (<http://www.electronconsulting.com/rsa-haskell>).
--
-- As the original code is GPL, this has to be.
-- This code is free software; you can redistribute it and\/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This code is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this code; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111\-1307  USA
-----------------------------------------------------------------------------

module Codec.Encryption.RSA(
  -- * Function Types
  encrypt,
  decrypt
  )where

import Codec.Utils
import Codec.Encryption.RSA.NumberTheory

rsaep :: (Integer , Integer) -> Integer -> Integer
rsaep (n,e) m 
   | m < 0 || m > n-1 = 
        error "Codec.Encryption.RSA.rsaep: message too long"
   | otherwise = 
        expmod m e n

-- | Take the modulus of the RSA key and the public exponent expressed
-- as lists of octets and the plaintext also expressed as a list of
-- octets and return the ciphertext as a list of octets. Of course,
-- these are all large integers but using lists of octets makes
-- everything easier. See 
-- <http://www.rsasecurity.com/rsalabs/pkcs/pkcs-1/index.html> for more
-- details.

encrypt :: ([Octet],[Octet]) -> [Octet] -> [Octet]
encrypt (n,e) m =
   i2osp (length n) $ 
    rsaep (fromOctets 256 n, fromOctets 256 e) (fromOctets 256 m)

rsadp :: (Integer , Integer) -> Integer -> Integer
rsadp (n,d) c 
   | c < 0 || c > n-1 = 
        error "Codec.Encryption.RSA.rsadp: decryption error"
   | otherwise = 
        expmod c d n

-- | Take the modulus of the RSA key and the private exponent expressed
-- as lists of octets and the ciphertext also expressed as a list of
-- octets and return the plaintext as a list of octets.

decrypt :: ([Octet],[Octet]) -> [Octet] -> [Octet]
decrypt (n,e) m =
   i2osp (length n) $ 
   rsadp (fromOctets 256 n, fromOctets 256 e) (fromOctets 256 m)

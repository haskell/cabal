-----------------------------------------------------------------------------
-- |
-- Module      :  Coded.Encryption.RSA.EMEOAEP
-- Copyright   :  (c) David J. Sankel 2003, Dominic Steinitz 2003
-- License     :  GPL (see the file ReadMe.tex)
--
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A modified version of the EMEOAEP module supplied by David J. Sankel
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

module Codec.Encryption.RSA.EMEOAEP(
   -- * Function Types
   encode, 
   decode
   )where

import Codec.Utils (Octet)
import Data.Bits

xorOctets :: Bits a => [a] -> [a] -> [a]
xorOctets = zipWith xor

-- | Take a mask generating function, a hash function, a label (which may be
--   null), a random seed, the modulus of the key and the message and returns
--   an encoded message. NB you could pass in the length of the modulus
--   but it seems safer to pass in the modulus itself and calculate the
--   length when required. See 
--   <ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1.pdf> for more
--   details.

encode :: (([Octet] -> [Octet]) -> [Octet] -> Int -> [Octet]) -> 
          ([Octet] -> [Octet]) -> [Octet] -> [Octet] -> [Octet] -> [Octet] ->
	  [Octet]

encode mgf hash p seed n m =
   if  length m > emLen - 2*hLen - 2
     then error "Codec.Encryption.EMEOAEP.encode: message too long"
     else em 
        where 
	   emLen      = length n
           mLen       = length m
           ps         = take (emLen-mLen-2*hLen-2) $ repeat $ 0x00
           pHash      = hash p
           hLen       = length pHash
           db         = pHash ++ ps ++ [0x01] ++ m
           dbMask     = mgf hash seed (length db)
           maskedDB   = db `xorOctets` dbMask
           seedMask   = mgf hash maskedDB hLen
           maskedSeed = seed `xorOctets` seedMask
           em         = [0x00] ++ maskedSeed ++ maskedDB

-- | Take a mask generating function, a hash function, a label (which may be
--   null) and the message and returns the decoded.

decode :: (([Octet] -> [Octet]) -> [Octet] -> Int -> [Octet]) -> 
          ([Octet] -> [Octet]) -> [Octet] -> [Octet] -> [Octet]

decode mgf hash p em =
  if length em < 2*hLen + 1 || 
     one /= 0x01 || 
     pHash' /= pHash ||
     y /= [0x00]
     then error "Codec.Encryption.EMEOAEP.decode: decryption error"
     else m
        where
	   (y,rest)              = splitAt 1 em
           pHash                 = hash p
	   hLen                  = length pHash
           (maskedSeed,maskedDB) = splitAt hLen rest
           seedMask              = mgf hash maskedDB hLen
           seed                  = maskedSeed `xorOctets` seedMask
           emLen                 = length em
           dbMask                = mgf hash seed (emLen - hLen - 1)
           db                    = maskedDB `xorOctets` dbMask
           (pHash',rest')        = splitAt hLen db
           (one:m)               = dropWhile (== 0x00) rest'

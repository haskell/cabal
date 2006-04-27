-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.RSA.MGF
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Implements the mask generation function as specified in:
-- <ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1.pdf>
--
-----------------------------------------------------------------------------

module Codec.Encryption.RSA.MGF (
   -- * Function Types
   mgf) where

import Codec.Utils (Octet, i2osp)

-- | Take a hash function, a seed and the intended length of the
--   the mask and deliver a mask of the requested length.

mgf :: ([Octet] -> [Octet]) -> [Octet] -> Int -> [Octet]

mgf hash z l =
   take l $ concat $ hashes
      where
         hashes = map f [0..(l `div` hLen)]
         hLen   = length $ f 0
	 f      = hash . (z++) . (i2osp 4)

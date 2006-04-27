-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.Padding
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Padding algorithms for use with block ciphers.
--
-- This module currently supports:
--
-- * PKCS5 padding and unpadding.
--
-- * Null padding and unpadding.
-- 
-----------------------------------------------------------------------------

module Codec.Encryption.Padding (
   -- * Function types
   pkcs5, unPkcs5,
   padNulls, unPadNulls, testPad
   ) where

import Data.Word
import Data.Bits
import Data.List
import Codec.Utils

-- | When the last block of plaintext is shorter than the block size then it
-- must be padded. PKCS5 specifies that the padding octets should each 
-- contain the number of octets which must be stripped off. So, for example,
-- with a block size of 8, \"0a0b0c\" will be padded with \"05\" resulting in
-- \"0a0b0c0505050505\". If the final block is a full block of 8 octets
-- then a whole block of \"0808080808080808\" is appended.

pkcs5 :: (Integral a, Bits a) => [Octet] -> [a]
pkcs5 s = pad p s where p n = replicate n (fromIntegral n)

-- | When the last block of plaintext is shorter than the block size then it
-- must be padded. Nulls padding specifies that the padding octets should each 
-- contain a null. So, for example,
-- with a block size of 8, \"0a0b0c\" will be padded to
-- \"0a0b0c0000000000\". If the final block is a full block of 8 octets
-- then a whole block of \"0000000000000000\" is appended.
-- NB this is only suitable for data which does not contain nulls,
-- for example, ASCII.

padNulls :: (Integral a, Bits a) => [Octet] -> [a]
padNulls s = pad p s where p n = replicate n 0

testPad s = pad p s where p n = replicate (n-1) 0xff ++ [fromIntegral n]

pad p s =
   blocks where
      octetSize = (bitSize $ head blocks) `div` 8
      blocks = map (fromOctets 256) (unfoldr h $ concat $ unfoldr g s) 
      g :: [Octet] -> Maybe ([Octet],[Octet])
      g x 
         | l == 0         = Nothing
         | l <  octetSize = Just (t ++ (p (octetSize-l)), [])
         | d == []        = Just (t ++ (p octetSize), [])
         | otherwise      = Just (t, d)
         where l   = length t
               t   = take octetSize x
	       d   = drop octetSize x
      h :: [Octet] -> Maybe ([Octet],[Octet])
      h x 
         | x == []   = Nothing
         | otherwise = Just (take octetSize x, drop octetSize x)

-- | Take a list of blocks padded using the method described in PKCS5
-- (see <http://www.rsasecurity.com/rsalabs/pkcs/pkcs-5>)
-- and return the list of unpadded octets. NB this function does not
-- currently check that the padded block is correctly formed and should
-- only be used for blocks that have been padded correctly.

unPkcs5 :: (Bits a, Integral a) => [a] -> [Octet]
unPkcs5 s = 
   unPad h s 
      where 
         h octetSize x = take (octetSize - (fromIntegral (last x))) x

-- | Take a list of blocks padded with nulls
-- and return the list of unpadded octets. NB if the blocks contain
-- a null then the result is unpredictable.

unPadNulls :: (Bits a, Integral a) => [a] -> [Octet]
unPadNulls s = 
   unPad h s
      where
         h _ x = takeWhile (/=0) x

unPad p s =
   concat $ unfoldr g s where
      g :: (Integral a, Bits a) => [a] -> Maybe ([Octet],[a])
      g x 
         | t == []   = Nothing
         | d == []   = Just (s, [])
         | otherwise = Just (v, d) 
	 where t     = take 1 x
               d     = drop 1 x
               u     = head t
               octetSize = (bitSize u) `div` 8
	       v     = i2osp octetSize u
	       s     = p octetSize v

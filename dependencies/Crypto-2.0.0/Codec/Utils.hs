-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Utils
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for coding and decoding.
--
-----------------------------------------------------------------------------

module Codec.Utils (
   -- * Types and Constants
   Octet,
   msb,
   -- * Octet Conversion Functions
   fromTwosComp, toTwosComp,
   toOctets, fromOctets,
   i2osp
	      ) where

import Data.Word
import Data.Bits

powersOf n = 1 : (map (*n) (powersOf n))

toBase x = 
   map fromIntegral .
   reverse .
   map (flip mod x) .
   takeWhile (/=0) .
   iterate (flip div x)

-- | Take a number a convert it to base n as a list of octets.

toOctets :: (Integral a, Integral b) => a -> b -> [Octet]
toOctets n x = (toBase n . fromIntegral) x

-- | The basic type for encoding and decoding.

type Octet = Word8

-- | The most significant bit of an 'Octet'.

msb :: Int
msb = bitSize (undefined::Octet) - 1

-- | Take a list of octets (a number expressed in base n) and convert it
--   to a number.

fromOctets :: (Integral a, Integral b) => a -> [Octet] -> b
fromOctets n x = 
   fromIntegral $ 
   sum $ 
   zipWith (*) (powersOf n) (reverse (map fromIntegral x))

-- | Take the length of the required number of octets and convert the 
--   number to base 256 padding it out to the required length. If the
--   required length is less than the number of octets of the converted
--   number then return the converted number. NB this is different from
--   the standard <ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1.pdf>
--   but mimics how replicate behaves.

i2osp :: Integral a => Int -> a -> [Octet]
i2osp l y = 
   pad ++ z
      where
         pad = replicate (l - unPaddedLen) (0x00::Octet)
	 z = toOctets 256 y
	 unPaddedLen = length z

-- | Convert from twos complement.

fromTwosComp :: Integral a => [Octet] -> a
fromTwosComp x =  conv x
   where conv []       = 0
         conv w@(x:xs) = if (testBit x msb)
                            then neg w
                            else pos w
         neg w@(x:xs)  = let z=(clearBit x msb):xs in
                            fromIntegral((fromOctets 256 z)-
                                         (128*(256^((length w)-1))))
         pos w         = fromIntegral(fromOctets 256 w)

toTwosComp :: Integral a => a -> [Octet]
toTwosComp x
   | x < 0     = reverse . plusOne . reverse . (map complement) $ u
   | x == 0    = [0x00]
   | otherwise = u
   where z@(y:ys) = toBase 256 (abs x)
         u        = if testBit y msb
                       then 0x00:z
                       else z

plusOne :: [Octet] -> [Octet]
plusOne [] = [1]
plusOne (x:xs) =
   if x == 0xff
      then 0x00:(plusOne xs)
      else (x+1):xs

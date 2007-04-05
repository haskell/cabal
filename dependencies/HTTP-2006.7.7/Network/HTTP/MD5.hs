-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Digest.MD5
-- Copyright   :  (c) Dominic Steinitz 2004
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Takes the MD5 module supplied by Ian Lynagh and wraps it so it
-- takes [Octet] and returns [Octet] where the length of the result
-- is always 16.
-- See <http://web.comlab.ox.ac.uk/oucl/work/ian.lynagh/>
-- and <http://www.ietf.org/rfc/rfc1321.txt>.
--
-----------------------------------------------------------------------------

module Network.HTTP.MD5 (
   -- * Function Types
   hash) where

import Data.Char(chr)
import Data.List(unfoldr)
import Numeric(readHex)

import Network.HTTP.MD5Aux
import Data.Word (Word8)

type Octet = Word8

-- | Take [Octet] and return [Octet] according to the standard.
--   The length of the result is always 16 octets or 128 bits as required
--   by the standard.

hash :: [Octet] -> [Octet]
hash xs = 
   unfoldr f $ md5s $ Str $ map (chr . fromIntegral) xs
      where f :: String -> Maybe (Octet,String)
            f [] = 
               Nothing
            f (x:y:zs) = 
               Just (fromIntegral a,zs)
	       where [(a,_)] = readHex (x:y:[])

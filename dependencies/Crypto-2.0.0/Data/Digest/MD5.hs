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

module Data.Digest.MD5 (
   -- * Function Types
   hash) where

import Data.Digest.MD5Aux
import Codec.Utils
import Char(chr)
import List(unfoldr)
import Numeric(readHex)

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

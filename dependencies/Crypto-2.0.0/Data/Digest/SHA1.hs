-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Digest.SHA1
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Takes the SHA1 module supplied by Ian Lynagh and wraps it so it
-- takes [Octet] and returns [Octet] where the length of the result
-- is always 20.
-- See <http://web.comlab.ox.ac.uk/oucl/work/ian.lynagh/>
-- and <http://www.itl.nist.gov/fipspubs/fip180-1.htm>.
--
-----------------------------------------------------------------------------

module Data.Digest.SHA1 (
   -- * Function Types
   hash) where

import Data.Digest.SHA1Aux
import Codec.Utils
import Char(chr)
import List(unfoldr)
import Numeric(readHex)

-- | Take [Octet] and return [Octet] according to the standard.
--   The length of the result is always 20 octets or 160 bits as required
--   by the standard.

hash :: [Octet] -> [Octet]
hash xs = 
   unfoldr f $ sha1 $ map (chr . fromIntegral) xs
      where f :: String -> Maybe (Octet,String)
            f [] = 
               Nothing
            f (x:y:zs) = 
               Just (fromIntegral a,zs)
	       where [(a,_)] = readHex (x:y:[])

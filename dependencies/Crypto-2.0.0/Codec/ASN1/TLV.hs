-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.ASN1.TLV
-- Copyright   :  (c) Dominic Steinitz 2005
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Decode binary BER into abstract an abstract representation of tag,
-- length and value ensuring that the lengths are consistent.
--
-----------------------------------------------------------------------------

module Codec.ASN1.TLV (
   -- * Types

   -- * Function types,
   tlv,
   tlvIO
	      ) where
import Data.Bits
import Control.Exception
import Control.Monad.State
import Control.Monad.Error
import System.IO.Error
import qualified NewBinary.Binary as B (BinHandle, getBits, getByte)
import Codec.ASN1.BER
import Codec.Utils

-- The bit decoder will (by design) never lie about lengths
-- because it can check these. It may lie (if that's what it's being
-- told) about numbers of components because it can't check these 
-- without having the ASN.1 definitions.

{-
041120 125723
When interpreting the bits into tag-length-value form,
remember to keep track of where things are.

041120 161608
We'll need isEOFBin or to catch exceptions.

041121 151059
As decoding to tag length value is monadic and then so it
decoding to the ASN.1 at some point we'll need to interleave
actions.

080105 082425
tlv should report an error if there are any Octets left over.
-}

tlv :: [Octet] -> (Length,Encoding)
tlv xs = let ((l,e),_) = runState (tlv_ undefined) (xs,0::Offset) in (l,e)

tlvIO :: BinHandle -> IO (Length, Encoding)
tlvIO = tlv_

tlv_ bin =
   do tagValueVal <- getBits bin 5
      tagConstructionVal <- getBits bin 1
      tagTypeVal <- getBits bin 2
      let tagType = toEnum $ fromIntegral tagTypeVal
          tagValue = fromIntegral tagValueVal
      if tagValue /= 31
         then do (ll,l) <- getLength bin
                 f 1 tagConstructionVal 
                   tagType tagValue ll l
         else do xs <- getTagOctets bin
                 let longform = 
                        fromIntegral (fromOctets 128 xs)
                 (ll,l) <- getLength bin
                 f (fromIntegral $ length xs) tagConstructionVal 
                   tagType longform ll l
   where f tl tcv tt tv ll l = 
            if tcv == 0
               then do xs <- getOctets bin l
                       let x = Primitive tt tv l xs
                       return (tl+ll+l,x)
               else do ys <- tlvs_ bin l
                       let x = Constructed tt tv l ys
                       return (tl+ll+l,x)

tlvs_ bin curLen
   | curLen < 0  = fail "Codec.ASN1.TLV.tlvs_: trying to decode a negative number of octets"
   | curLen == 0 = return []
   | otherwise   = do (l,x)  <- tlv_ bin
                      ys     <- tlvs_ bin (curLen-l)
                      return (x:ys)

getTagOctets bin = 
   do x <- getByte bin
      if not (testBit x msb)
         then return [x]
         else do xs <- getTagOctets bin
                 return ((clearBit x msb):xs)

-- Need to think about testing. Here are some links:

-- http://www.eeye.com/html/Research/Advisories/AD20040210.html
-- http://www.galois.com/files/HCSS-04-ASN.1.pdf
-- http://www.larmouth.demon.co.uk/tutorials/tagging/sld003.htm


getLength bin =
   do x <- getByte bin
      let isShort   = not (testBit x msb)
          shortform = fromIntegral x
          length    = fromIntegral (clearBit x msb) in 
         if x == 0x80
            then error "Indefinite length not supported"
            else if isShort
               then return (1,shortform)
               else do xs <- getOctets bin length
                       let longform = fromOctets 256 xs in
                          return (length+1,longform)

getOctets bin l = 
   if l <= 0 
      then return []
      else do x  <- getByte bin
              xs <- getOctets bin (l-1)
              return (x:xs)

type BinHandle = B.BinHandle

type NumBits = Int

class Binary m where
   getBits :: BinHandle -> NumBits -> m Octet
   getByte :: BinHandle -> m Octet

instance Binary IO where
   getBits = B.getBits
   getByte = B.getByte

type Offset  = Int

instance Binary (State ([Octet],Offset)) where
   getBits = getBits'
   getByte = getByte'

{-
getBits is never exported and does not need to be general. We know
we will only ever use it at an Octet boundary and we will never cross
an Octet boundary.
-}

getBits' :: MonadState ([Octet],Offset) m => BinHandle -> NumBits -> m Octet
getBits' _ n =
   do (xs,offset) <- get
      if null xs
         then throw (IOException $ 
                     mkIOError eofErrorType 
                               "Codec.ASN1.TLV.getBits" 
                               Nothing Nothing)
         else do let r = select offset n (head xs)
                     m = bitSize r 
                 if offset + n < m
                    then put (xs,offset + n)
                    else put (tail xs,0)
                 return r

select :: Offset -> NumBits -> Octet -> Octet
select offset n x = 
   clearBits n p $ shiftR x offset
   where p = bitSize n

clearBits :: Bits a => Int -> Int -> a -> a
clearBits = bits clearBit

bits :: Enum b => (a -> b -> a) -> b -> b -> a -> a
bits f m n = foldr (.) id (map (\i -> flip f i)  [m..n]) 

getByte' :: MonadState ([Octet],Offset) m => BinHandle -> m Octet
getByte' _ =
   do (xs,offset) <- get
      if null xs
         then throw (IOException $ 
                     mkIOError eofErrorType 
                               "Codec.ASN1.TLV.getByte" 
                               Nothing Nothing)
         else do put (tail xs,offset)
                 return (head xs)

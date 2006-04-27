-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.ASN1.BER
-- Copyright   :  (c) Dominic Steinitz 2005
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Typecheck and decode BER representations as produced by
-- Codec.ASN1.TLV 
--
-----------------------------------------------------------------------------

module Codec.ASN1.BER (
   -- * Types
   Encoding(..),
   Length,
   -- * Type classes
   Encode(..),
   -- * Function types
   encodedComponents,
   encodedDefComps,
   tc,
   replaceRef
	      ) where

import Char
import Bits
import Data.List
import Control.Monad.Error
import Control.Monad.State
import Data.FiniteMap
import Codec.Utils
import Codec.ASN1

type Length = Integer
type PrimitiveValue = [Octet]

data Encoding = Primitive TagType TagValue Length PrimitiveValue
              | Constructed TagType TagValue Length [Encoding]
   deriving (Eq,Show)

data Defaulted = DefPrim TagType TagValue Length PrimitiveValue
              | DefCons TagType TagValue Length [Maybe Defaulted]
   deriving (Eq,Show)

encodedComponents :: Encoding -> [Encoding]
encodedComponents (Constructed _ _ _ es) = es

encodedDefComps :: Defaulted -> [Maybe Defaulted]
encodedDefComps (DefCons _ _ _ es) = es


-- | Type check the abstract representation of a Tag Length Value
--   against an ASN.1 type definition.

tc :: MonadError e m => TypeDefn -> Encoding -> m Defaulted

tc (n ::= AbsBasePrim att atv at) b@(Primitive btt btv l bv) 
   | att /= btt = tagMismatch n att btt
   | atv /= btv = tagValMismatch n atv btv
   | not $ bv `compatibleWith` at = 
        fail ("Checking " ++ (show n) ++ ": " ++
              "type not compatible with values " ++ (show bv))
   | otherwise = return $ DefPrim btt btv l bv

tc (n ::= AbsBasePrim att atv at) (Constructed btt btv _ bv) 
   = fail ("Checking " ++ (show n) ++ ": " ++
           "expected PRIMITIVE Tag found CONSTRUCTED Tag")

-- See x.690 8.14.2 & 8.14.3

tc (n ::= AbsRef att atv atp at) b@(Primitive btt btv _ bv)
   | atp == Explicit = 
        fail ("Checking " ++ (show n) ++ ": " ++
              "expected IMPLICIT Tag found PRIMITIVE type")
   | att /= btt = tagMismatch n att btt
   | atv /= btv = tagValMismatch n atv btv
   | otherwise = tc a b
    where a = modName n $ modTagType att $ modTagVal (Just atv) at

tc (n ::= AbsRef att atv atp at) b@(Constructed btt btv _ bvs)
   | att /= btt = tagMismatch n att btt
   | atv /= btv = tagValMismatch n atv btv
   | otherwise = case atp of
                    Implicit -> tc a b
                    Explicit -> if null bvs
                                   then fail "unable to match empty value"
                                   else tc (modName n at) (bvs!!0)
    where a = modName n $ modTagType att $ modTagVal (Just atv) at

tc (n ::= AbsSeq _ _ _ _) (Primitive _ _ _ _) =
   constructionMismatch n "SEQUENCE" "PRIMITIVE"

tc (n ::= AbsSeq att atv atp as) (Constructed btt btv l bvs)
   | att /= btt = tagMismatch n att btt
   | atv /= btv = tagValMismatch n atv btv
--    | bLength < minLength || bLength > maxLength =
--         fail ("Checking " ++ (show n) ++ ": " ++
--               "expected " ++ (show $ minLength) ++ " - " ++ 
--                              (show $ maxLength) ++ " components " ++
--               "found " ++ (show $ bLength))
   | otherwise = 
        do (bar,s) <- runStateT (k as bvs) []
{-
           let x = head bar
               (Just (DefPrim _ _ _ y)) = x
               z = head y
           if z /= 104 
              then error (show s)
              else return (DefCons btt btv l bar)
-}
           return (DefCons btt btv l bar)
    where opts = map h as
          h (Regular _) = False
          h _ = True
          minLength = length $ filter (==False) opts
          maxLength = length opts
          bLength = length bvs
          
tc (n ::= AbsSeqOf _ _ _ _) (Primitive _ _ _ _) =
   constructionMismatch n "SEQUENCE OF" "PRIMITIVE"

tc (n ::= AbsSeqOf att atv Implicit td) (Constructed btt btv l bvs)
   | att /= btt = tagMismatch n att btt
   | atv /= btv = tagValMismatch n atv btv
   | otherwise = do ds <- sequence $ zipWith tc (repeat td) bvs
                    return (DefCons btt btv l (map Just ds))

tc (n ::= AbsSetOf _ _ _ _) (Primitive _ _ _ _) =
   constructionMismatch n "SET OF" "PRIMITIVE"

tc (n ::= AbsSetOf att atv Implicit td) (Constructed btt btv l bvs)
   | att /= btt = tagMismatch n att btt
   | atv /= btv = tagValMismatch n atv btv
   | otherwise = do ds <- sequence $ zipWith tc (repeat td) bvs
                    return (DefCons btt btv l (map Just ds))                    
k :: MonadError e m => 
        [ComponentType] -> [Encoding] -> 
           StateT [Maybe Encoding] m [Maybe Defaulted]

k [] [] = return []

k [] _  = return []

k ((a@(Regular _)):_) []  = 
   fail ("Checking " ++ (show a) ++ ": " ++ "insufficient components")

k (a@(AnyDefBy n):as) [] =
   fail ("Checking " ++ (show a) ++ ": " ++ "insufficient components")

k (Optional _:_) [] = return [Nothing]

k (Default _ _:_) [] = fail "To be fixed"

k (Regular (mn :>: (tv :@: td)):as) (bv:bvs) = 
   do foo <- lift $ case tv of
                Nothing ->
                   tc td bv
                Just v ->
                   case mn of
-- 29/01/05 082427 Consider replacing Maybe String by String.
-- If there is no name then it's the empty String "".
                      Nothing ->
	                 tc ("" ::= AbsRef Context v Explicit td) bv
                      Just name ->
                         tc (name ::= AbsRef Context v Explicit td) bv
      s <- get
      put (Just bv:s)
      baz <- k as bvs
      return ((Just foo):baz)

k (Optional (mn :>: (tv :@: td)):as) b@(bv:bvs) = 
-- For the moment. We don't want to catch all errors. For example,
-- if we get an eof error then it should be propogated.
   do optionPresent <- 
         (do foo <- lift $ case tv of
                       Nothing ->
                          tc td bv
                       Just v ->
                          case mn of
-- 29/01/05 082427 Consider replacing Maybe String by String.
-- If there is no name then it's the empty String "".
                             Nothing ->
	                        tc ("" ::= AbsRef Context v Explicit td) bv
                             Just name ->
                                tc (name ::= AbsRef Context v Explicit td) bv
             return (Just foo)) `catchError`
         (\_ -> return Nothing)
      case optionPresent of
         Nothing ->
            do s <- get
               put (Nothing:s)
               baz <- k as b
               return (optionPresent:baz)
         (Just _) ->
            do s <- get
               put (Just bv:s)
               baz <- k as bvs
               return (optionPresent:baz)

k (Default (mn :>: (tv :@: td)) _:as) b@(bv:bvs) = 
-- For the moment. We don't want to catch all errors. For example,
-- if we get an eof error then it should be propogated.
   do optionPresent <- 
         (do foo <- lift $ case tv of
                       Nothing ->
                          tc td bv
                       Just v ->
                          case mn of
-- 29/01/05 082427 Consider replacing Maybe String by String.
-- If there is no name then it's the empty String "".
                             Nothing ->
	                        tc ("" ::= AbsRef Context v Explicit td) bv
                             Just name ->
                                tc (name ::= AbsRef Context v Explicit td) bv
             return (Just foo)) `catchError`
         (\_ -> return Nothing)
      case optionPresent of
         Nothing ->
            do s <- get
               put (Nothing:s) -- This is wrong. We should insert the default.
               baz <- k as b
               return (optionPresent:baz)
         (Just _) ->
            do s <- get
               put (Just bv:s)
               baz <- k as bvs
               return (optionPresent:baz)

k ((AnyDefBy n):as) (bv:bvs) =
   do s <- get
      if ((s!!n) == Nothing)
         then fail ("Checking " ++ (show n) ++ ": " ++
                     "no optional value present in ANY DEFINED BY")
         else do let (Just x) = (reverse s)!!n
                 y <- lift $ tc absOID x
                 let u = decode (getAbsType absOID) (Just y)
                     (Just u') = u
                     v = lookupFM oids u'
                 if v == Nothing 
                    then fail ("Checking " ++ (show n) ++ ": " ++
                               (show u) ++ " not supported")
                    else do let (Just w) = v
                            foo <- lift $ tc w bv
                            s <- get
                            put (Just bv:s)
                            baz <- k as bvs
                            return (Just foo:baz)
      
compatibleWith :: PrimitiveValue -> AbsPrimType -> Bool
compatibleWith pv AbsVisibleString = 
   all (flip elem visibleOctets) pv
compatibleWith pv AbsPrintableString =
   all (flip elem printableOctets) pv
compatibleWith pv AbsIA5String =
   all (flip elem ia5Octets) pv   
compatibleWith pv AbsBool = 
   length pv == 1 
compatibleWith pv AbsInteger =
   if length pv > 1
      then not ((pv!!0 == 0xff && (testBit (pv!!1) msb)) ||
                (pv!!0 == 0x00 && (not (testBit (pv!!1) msb))))
      else length pv == 1
compatibleWith pv AbsOID = not $ null pv
compatibleWith pv AbsOctetString = True
compatibleWith pv AbsBitString = True
compatibleWith pv AbsNull = null pv

ia5Octets :: [Octet]
ia5Octets = [0..127]

visibleOctets :: [Octet]
visibleOctets = map fromIntegral [ord ' '..ord '~']

printableOctets :: [Octet]
printableOctets = 
   map (fromIntegral . ord) printableString

printableString =
   ['A'..'Z'] ++
   ['0'..'9'] ++
   [' ']      ++
   ['a'..'z'] ++
   ['\'']     ++
   ['(']      ++
   [')']      ++
   ['+']      ++
   [',']      ++
   ['-']      ++
   ['.']      ++
   ['/']      ++
   [':']      ++
   ['=']      ++
   ['?'] 

tagMismatch n a b =
   fail ("Checking " ++ (show n) ++ ": " ++
         "expected tag type " ++ (show a) ++ " " ++
         "found tag type " ++ (show b))

tagValMismatch n a b =
   fail ("Checking " ++ (show n) ++ ": " ++
         "expected tag value " ++ (show a) ++ " " ++
         "found tag value " ++ (show b))

constructionMismatch n sa sb = 
   fail ("Checking " ++ (show n) ++ ": " ++
         "unable to match " ++ sa ++ " with " ++ sb)

decodeMismatch a b =
   fail ("Panic: unable to decode " ++ (show b) ++ " with " ++ (show a)) 

class Encode a where
   decode :: AbstractType -> Maybe Defaulted -> Maybe a

instance Encode VisibleString where
   decode a@(AbsBasePrim _ _ AbsVisibleString) b = 
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return $ VisibleString $ map (chr . fromIntegral) bv
            _ ->
               decodeMismatch a b

instance Encode PrintableString where
   decode a@(AbsBasePrim _ _ AbsPrintableString) b = 
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return $ PrintableString $ map (chr . fromIntegral) bv
            _ ->
               decodeMismatch a b

instance Encode IA5String where
   decode a@(AbsBasePrim _ _ AbsIA5String) b = 
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return $ IA5String $ map (chr . fromIntegral) bv
            _ ->
               decodeMismatch a b

instance Encode DirectoryString where
   decode a@(AbsBasePrim _ _ AbsIA5String) b = 
      do x <- decode a b
         return (IA x)
   decode a@(AbsBasePrim _ _ AbsPrintableString) b = 
      do x <- decode a b      
         return (PS x)
   decode a@(AbsBasePrim _ _ AbsVisibleString) b = 
      do x <- decode a b      
         return (VS x)

instance Encode Bool where
   decode a@(AbsBasePrim _ _ AbsBool) b =
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               case bv of
                  [0x00]    -> return False
                  otherwise -> return True
            _ ->
               decodeMismatch a b

instance Encode Integer where
   decode a@(AbsBasePrim _ _ AbsInteger) b =
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return (fromTwosComp bv)
            _ ->
               decodeMismatch a b         

instance Encode OctetString where
   decode a@(AbsBasePrim _ _ AbsOctetString) b =
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return $ OctetString bv      
            _ ->
               decodeMismatch a b

instance Encode BitString where
   decode a@(AbsBasePrim _ _ AbsBitString) b =
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return $ BitString (tail bv) 
-- For now. Typechecking will have to ensure this is valid.   
            _ ->
               decodeMismatch a b

instance Encode a => Encode (SetOf a) where
   decode a b = 
      do d <- b
         let bs = encodedDefComps d
         cs <- f a' bs
         return $ SetOf cs
      where a' = absSetOfType a
            f x ys = 
               case ys of
                  [] ->
                     return $ []
                  (z:zs) ->
                     do u <- decode x z
                        us <- f x zs
                        return $ (u:us) 

instance Encode a => Encode [a] where
   decode a b = 
      do d <- b
         let bs = encodedDefComps d
         cs <- f a' bs
         return cs
      where a' = absSeqOfType a
            f x ys = 
               case ys of
                  [] ->
                     return $ []
                  (z:zs) ->
                     do u <- decode x z
                        us <- f x zs
                        return $ (u:us) 

instance Encode OID where
   decode a@(AbsBasePrim _ _ AbsOID) b =
      do x <- b
         case x of
            DefPrim _ _ _ bv ->
               return $ decodeOIDAux bv
            _ ->
               decodeMismatch a b

decodeOIDAux (x:xs) = 
   OID $ ((fromIntegral x) `div` 40):((fromIntegral x) `mod` 40):ys
      where
         ys = map fromIntegral $
	      map (fromOctets (2^oidBitsPerOctet)) $
	      (map . map) (flip clearBit oidBitsPerOctet) (subIds xs)
         subIds :: [Octet] -> [[Octet]]
         subIds = unfoldr getSubId
         getSubId :: [Octet] -> Maybe ([Octet], [Octet])
         getSubId [] = Nothing
         getSubId xs = Just $ span' endOfSubId xs
         endOfSubId = not . (flip testBit oidBitsPerOctet)

oidBitsPerOctet = 7 :: Int

span' :: (a -> Bool) -> [a] -> ([a],[a])
span' p []
   = ([],[])
span' p xs@(x:xs') 
   | p x       = ([x],xs') 
   | otherwise = (x:ys,zs)
      where (ys,zs) = span' p xs'

replaceRef :: AbstractType -> 
              [AbstractType] -> 
              [Maybe Defaulted] -> 
              AbstractType
replaceRef a as bs = 
   case a of
      AbsAnyDefBy n -> u
         where
            oidat = decode (as!!n) (bs!!n)
            (Just oidat') = oidat
            t     = lookupFM oids oidat'
            (Just (_ ::= u)) = t
      _ -> a

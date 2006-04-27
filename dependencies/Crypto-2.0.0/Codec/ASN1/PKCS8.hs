-----------------------------------------------------------------------------
-- |
-- Module      :  Coded.ASN1.PKCS8
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Definitions to allow the typechecking of a PKCS8 private key and
-- functions to extract information from it. 
-- 
-- See <http://www.rsasecurity.com/rsalabs/pkcs/pkcs-8/>.
--
-----------------------------------------------------------------------------

module Codec.ASN1.PKCS8 (
-- * Type declarations
   RSAPrivateKey(..),
   PrivateKeyInfo(..),
-- * Function declarations
   rsaPrivateKey,
   privateKeyInfo
   ) where

import Data.Maybe
import Codec.ASN1
import Codec.ASN1.BER
import Codec.ASN1.X509 (
   algorithmIdentifier,
   AlgorithmIdentifier,
   attributeTypeAndValue,
   AttributeTypeAndValue
   )

{-
See http://www.zvon.org/tmRFC/RFC3447/Output/index.html 10.1.2. A.1.2
RSA private key syntax:

RSAPrivateKey ::= SEQUENCE {
   version           Version,
   modulus           INTEGER,  -- n
   publicExponent    INTEGER,  -- e
   privateExponent   INTEGER,  -- d
   prime1            INTEGER,  -- p
   prime2            INTEGER,  -- q
   exponent1         INTEGER,  -- d mod (p-1)
   exponent2         INTEGER,  -- d mod (q-1)
   coefficient       INTEGER,  -- (inverse of q) mod p
   otherPrimeInfos   OtherPrimeInfos OPTIONAL
      }
-}

rsaPrivateKey :: TypeDefn
rsaPrivateKey =
   "RSAPrivateKey" ::=
      AbsSeq Universal 16 Implicit 
         [Regular (Just "version"         :>: (Nothing :@: version)),
          Regular (Just "modulus"         :>: (Nothing :@: absInteger)),
          Regular (Just "publicExponent"  :>: (Nothing :@: absInteger)),
          Regular (Just "privateExponent" :>: (Nothing :@: absInteger)),
          Regular (Just "prime1"          :>: (Nothing :@: absInteger)),
          Regular (Just "prime2"          :>: (Nothing :@: absInteger)),
          Regular (Just "exponent1"       :>: (Nothing :@: absInteger)),
          Regular (Just "exponent2"       :>: (Nothing :@: absInteger)),
          Regular (Just "coefficient"     :>: (Nothing :@: absInteger))]

data RSAPrivateKey =
   RSAPrivateKey {
      version1        :: Integer,
      modulus         :: Integer, -- n
      publicExponent  :: Integer, -- e
      privateExponent :: Integer, -- d
      prime1          :: Integer, -- p
      prime2          :: Integer, -- q
      exponent1       :: Integer, -- d mod (p-1)
      exponent2       :: Integer, -- d mod (q-1)
      coefficient     :: Integer  -- (inverse of q) mod p
      }
   deriving Show

instance Encode RSAPrivateKey where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
             version         = fromJust $ decode (as'!!0) (bs!!0)
             modulus         = fromJust $ decode (as'!!1) (bs!!1)
             publicExponent  = fromJust $ decode (as'!!2) (bs!!2)
             privateExponent = fromJust $ decode (as'!!3) (bs!!3)
             prime1          = fromJust $ decode (as'!!4) (bs!!4)
             prime2          = fromJust $ decode (as'!!5) (bs!!5)
             exponent1       = fromJust $ decode (as'!!6) (bs!!6)
             exponent2       = fromJust $ decode (as'!!7) (bs!!7)
             coefficient     = fromJust $ decode (as'!!8) (bs!!8)
         return $ 
            RSAPrivateKey {
               version1        = version,
               modulus         = modulus,
               publicExponent  = publicExponent,
               privateExponent = privateExponent,
               prime1          = prime1,
               prime2          = prime2,
               exponent1       = exponent1,
               exponent2       = exponent2,
               coefficient     = coefficient}

version = modName "Version" absInteger

type Version = Integer

{-
PrivateKey ::= OCTET STRING
-}

privateKey = modName "PrivateKey" absOctetString

type PrivateKey = OctetString

{-
Attributes ::= SET OF Attribute
-}

attributes = 
   "Attributes" ::= AbsSetOf Universal 16 Implicit attributeTypeAndValue

type Attributes = SetOf AttributeTypeAndValue

{-
PrivateKeyInfo ::= SEQUENCE {
   version Version,
   privateKeyAlgorithm AlgorithmIdentifier {{PrivateKeyAlgorithms}},
   privateKey PrivateKey,
   attributes [0] Attributes OPTIONAL 
      }
-}

privateKeyInfo :: TypeDefn
privateKeyInfo =
   "privateKeyInfo" ::=
      AbsSeq Universal 16 Implicit [
         Regular  (Just "version"      :>: (Nothing :@: version)),
         Regular  (
            Just "privateKeyAlgorithm" :>: (Nothing :@: algorithmIdentifier)
         ),
         Regular  (Just "privateKey"   :>: (Nothing :@: privateKey)),
         Optional (Just "attributes"   :>: (Nothing :@: attributes))
      ]

data PrivateKeyInfo =
   PrivateKeyInfo {
      version2            :: Version,
      privateKeyAlgorithm :: AlgorithmIdentifier,
      privateKey1         :: PrivateKey,
      attributes1         :: Maybe Attributes
      }
   deriving Show

instance Encode PrivateKeyInfo where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
             version             = fromJust $ decode (as'!!0) (bs!!0)
             privateKeyAlgorithm = fromJust $ decode (as'!!1) (bs!!1)
             privateKey          = fromJust $ decode (as'!!2) (bs!!2)
             attributes          = decode (as'!!3) (bs!!3)
         return $ 
            PrivateKeyInfo {
               version2            = version,
               privateKeyAlgorithm = privateKeyAlgorithm,
               privateKey1         = privateKey,
               attributes1         = attributes
            }

{-
type Algorithm = OID

-- | This will do for now. DSA has some parameters which are more complicated
-- than this but since we plan to do RSA initially and this has NULL parameters
-- then anything will do to get us going.

type Parameters = Int

-- | The parameters will only ever be Nothing as this implementation
-- only supports RSA and this has no parameters. So even if the parameters
-- are non-NULL, fromASN will not fail but will ignore them.

data AlgorithmIdentifier =
   MkAlgorithmIdentifier {
      algorithm :: Algorithm,
      parameters :: Maybe Parameters } 
   deriving Show

data PrivateKeyInfo =
   MkPrivateKeyInfo {
      version1 :: Version,
      privateKeyAlgorithm :: AlgorithmIdentifier,
      privateKey :: RSAPrivateKey }
   deriving Show

{-
We are "overloading" Version. It is defined in 

ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-8/pkcs-8v1_2.asn:

Version ::= INTEGER {v1(0)} (v1,...)

and also in 

http://www.zvon.org/tmRFC/RFC3447/Output/index.html 10.1.2:

Version ::= INTEGER { two-prime(0), multi(1) } 

although for the latter
definition we represent two-prime in Haskell as V1 and do not support
multi.
-}

{-
We assume:

Algorithm ::= OID 

although the situation is far more complicated.
See http://www.zvon.org/tmRFC/RFC2898/Output/chapter12.html.
-}

algorithm = modName "Algorithm" absOID

type Algorithm = OID

{-
We assume:

Parameters ::= NULL

although the situation is far more complicated.
See http://www.zvon.org/tmRFC/RFC2898/Output/chapter12.html.
-}

parameters = modName "Parameters" absNull

type Parameters = NULL

{-
See http://www.itu.int/ITU-T/asn1/database/itu-t/x/x509/1997/AuthenticationFramework.html#AuthenticationFramework.AlgorithmIdentifier.

For now, the parameters will only ever be Nothing as this implementation
only supports RSA and this has no parameters. If the parameters
are non-NULL, we will report an error.

See http://www.zvon.org/tmRFC/RFC3447/Output/index.html 10.1. A.1 RSA key representation:

"The parameters field associated with this OID in a value of type AlgorithmIdentifier shall have a value of type NULL"
-}

{-
See ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-8/pkcs-8v1_2.asn.
-}

-}
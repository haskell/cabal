-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.ASN1.X509
-- Copyright   :  (c) Dominic Steinitz 2005
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Definitions to allow the typechecking of an X.509
-- certificate and functions to extract information from it.
--
-----------------------------------------------------------------------------

module Codec.ASN1.X509 (
   -- * Types
   Certificate(..),
   SignedCertificate(..),
   SubjectPublicKeyInfo(..),
   RSAPublicKey(..),
   AlgorithmIdentifier(..),
   AttributeTypeAndValue,
   -- * Type classes
   -- * Function types
   time,
   validity,
   attributeTypeAndValue,
   relativeDistinguishedName,
   algorithmIdentifier,
   signedCertificate,
   rsaPublicKey,
   algorithm1,
   parameters1,
   validity1,
   notBefore,
   notAfter,
   type1,
   value,
   unName,
   unTime,
   unRelativeDistinguishedName
) where
import System.Time
import Data.FiniteMap
import Data.Maybe
import Codec.ASN1.BER
import Codec.ASN1

{-
CertificateSerialNumber ::= INTEGER
-}

certificateSerialNumber = modName "CertificateSerialNumber" absInteger

type CertificateSerialNumber = Integer

{-
AttributeTypeAndValue ::=
   SEQUENCE {
      type  OBJECT IDENTIFIER,
      value ANY DEFINED by type
      }
-}

attributeTypeAndValue =
   "AttributeTypeAndValue" ::= 
      AbsSeq Universal 16 Implicit 
         [Regular (Just "type"  :>: (Nothing :@: absOID)),
          AnyDefBy 0]

data AttributeTypeAndValue = 
   AttributeTypeAndValue { type1  :: OID,
                           value  :: DirectoryString}
     deriving (Eq,Show)

instance Encode AttributeTypeAndValue where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            AttributeTypeAndValue {
               type1 = fromJust $ decode (as'!!0) (bs!!0),
               value = fromJust $ decode (as'!!1) (bs!!1)
            }

{-
RelativeDistinguishedName ::=
   SET OF AttributeTypeAndValue
-}

relativeDistinguishedName =
   "RelativeDistinguishedName" ::=
      AbsSetOf Universal 17 Implicit attributeTypeAndValue

data RelativeDistinguishedName = 
   RelativeDistinguishedName (SetOf AttributeTypeAndValue)
      deriving (Eq,Show)

-- unRelativeDistinguishedName :: 
--    RelativeDistinguishedName -> (OID,DirectoryString)
unRelativeDistinguishedName (RelativeDistinguishedName x) = x

instance Encode RelativeDistinguishedName where
   decode a b = 
      do x <- decode a b
         return (RelativeDistinguishedName x)

{-
Name ::= SEQUENCE OF RelativeDistnguishedName
-}

name =
   "Name" ::=
      AbsSeqOf Universal 16 Implicit relativeDistinguishedName

data Name = Name [RelativeDistinguishedName]
   deriving (Eq,Show)

unName :: Name -> [RelativeDistinguishedName]
unName (Name x) = x

instance Encode Name where
   decode a b = 
      do x <- decode a b
         return (Name x)

{-
Validity ::= 
   SEQUENCE {notBefore  Time,
             notAfter   Time
   }
-}

validity =
   "Validity" ::=
      AbsSeq Universal 16 Implicit
         [Regular (Just "notBefore"  :>: (Nothing :@: time)),
          Regular (Just "notAfter"   :>: (Nothing :@: time))]

data Validity =
   Validity {
      notBefore :: Time, -- CalendarTime
      notAfter  :: Time  -- CalendarTime 
      }
   deriving (Eq,Show)

instance Encode Validity where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            Validity {
               notBefore = fromJust $ decode (as!!0) (bs!!0),
               notAfter  = fromJust $ decode (as!!1) (bs!!1)
            }

{-
Time ::= 
   CHOICE {utcTime          UTCTime,
           generalizedTime  GeneralizedTime
   }
-}

time =
   "Time" ::= AbsRef Universal 23 Implicit absVisibleString

data Time = Time VisibleString
   deriving (Eq,Show)

unTime :: Time -> VisibleString
unTime (Time x) = x

instance Encode Time where
   decode a b = 
      do let (AbsRef _ _ _ a') = a 
             a'' = getAbsType a'
         x <- decode a'' b
         return $ Time x

{-
SubjectPublicKeyInfo ::= 
   SEQUENCE {
      algorithm         AlgorithmIdentifier,
      subjectPublicKey  BIT STRING
   }
-}

subjectPublicKeyInfo =
   "SubjectPublicKeyInfo" ::=
      AbsSeq Universal 16 Implicit
         [Regular (Just "algorithm"              :>: (Nothing :@: algorithmIdentifier)),
          Regular (Just "subjectPublicKeyInfo"   :>: (Nothing :@: absBitString))]

data SubjectPublicKeyInfo =
   SubjectPublicKeyInfo {
      algorithm2 :: AlgorithmIdentifier,
      subjectPublicKeyInfo1 :: BitString 
      }
   deriving (Eq,Show)

instance Encode SubjectPublicKeyInfo where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            SubjectPublicKeyInfo {
               algorithm2 = fromJust $ decode (as!!0) (bs!!0),
               subjectPublicKeyInfo1 = fromJust $ decode (as!!1) (bs!!1)
            }

{-
Certificate ::=
   SEQUENCE {
      version                 [0] Version DEFAULT v1,
      serialNumber                CertificateSerialNumber,
      signature                   AlgorithmIdentifier,
      issuer                      Name,
      validity                    Validity,
      subject                     Name,
      subjectPublicKeyInfo        SubjectPublicKeyInfo,
      issuerUniqueIdentifier  [1] IMPLICIT UniqueIdentifier OPTIONAL,
      -- if present, version shall be v2 or v3
      subjectUniqueIdentifier [2] IMPLICIT UniqueIdentifier OPTIONAL,
      -- if present, version shall be v2 or v3
      extensions              [3] Extensions OPTIONAL
      -- If present, version shall be v3 
   }
-}

certificate =
   "Certificate" ::=
      AbsSeq Universal 16 Implicit
         [Default (Just "version"     :>: 
             ((Just 0) :@: version)) [030200],
          Regular (Just "serialNumber"         :>: 
             (Nothing :@: certificateSerialNumber)),
          Regular (Just "signature"            :>: 
             (Nothing :@: algorithmIdentifier)),
          Regular (Just "issuer"               :>: 
             (Nothing :@: name)),
          Regular (Just "validity"             :>: 
             (Nothing :@: validity)),
          Regular (Just "subject"              :>: 
             (Nothing :@: name)),
          Regular (Just "subjectPublicKeyInfo" :>: 
             (Nothing :@: subjectPublicKeyInfo))]

data Certificate =
   Certificate {
      version3 :: Version,
      serialNumber :: CertificateSerialNumber,
      signature :: AlgorithmIdentifier,
      issuer :: Name,
      validity1 :: Validity,
      subject :: Name,
      subjectPublicKeyInfo2 :: SubjectPublicKeyInfo
      }
   deriving (Eq,Show)

instance Encode Certificate where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
             version              = fromJust $ decode (as!!0) (bs!!0)
             serialNumber         = fromJust $ decode (as!!1) (bs!!1)
             signature            = fromJust $ decode (as!!2) (bs!!2)
             issuer               = fromJust $ decode (as!!3) (bs!!3)
             validity             = fromJust $ decode (as!!4) (bs!!4)
             subject              = fromJust $ decode (as!!5) (bs!!5)
             subjectPublicKeyInfo = fromJust $ decode (as!!6) (bs!!6)
         return $ 
            Certificate {
               version3              = version,
               serialNumber          = serialNumber,
               signature             = signature,
               issuer                = issuer,
               validity1             = validity,
               subject               = subject,
               subjectPublicKeyInfo2 = subjectPublicKeyInfo
            }

signedCertificate =
   "SignedCertificate" ::=
      AbsSeq Universal 16 Implicit
         [Regular (Nothing :>: (Nothing :@: certificate)),
          Regular (Nothing :>: (Nothing :@: algorithmIdentifier)),
          Regular (Nothing :>: (Nothing :@: absBitString))]

data SignedCertificate =
   SignedCertificate {
      certificate1 :: Certificate,
      algorithmIdentifier1 :: AlgorithmIdentifier,
      octetString :: BitString
   }      
   deriving (Eq,Show)

instance Encode SignedCertificate where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            SignedCertificate {
               certificate1 = fromJust $ decode (as!!0) (bs!!0),
               algorithmIdentifier1 = fromJust $ decode (as!!1) (bs!!1),
               octetString = fromJust $ decode (as!!2) (bs!!2)
            }

{-
RSAPublicKey ::= SEQUENCE {
    modulus           INTEGER,  -- n
    publicExponent    INTEGER   -- e 
}
-}

rsaPublicKey =
   "RSAPublicKey" ::=
      AbsSeq Universal 16 Implicit [
         Regular (Just "modulus"        :>: (Nothing :@: absInteger)),
         Regular (Just "publicExponent" :>: (Nothing :@: absInteger))
      ]

data RSAPublicKey = 
   RSAPublicKey {
      modulus1 :: Integer,
      publicExponent1 :: Integer
   }
   deriving (Eq,Show)

instance Encode RSAPublicKey where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            RSAPublicKey {
               modulus1 = fromJust $ decode (as!!0) (bs!!0),
               publicExponent1 = fromJust $ decode (as!!1) (bs!!1)
            }

version = modName "Version" absInteger

type Version = Integer

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

algorithmIdentifier =
   "AlgorithmIdentifier" ::=
      AbsSeq Universal 16 Implicit 
         [Regular (Just "algorithm"  :>: (Nothing :@: algorithm)),      
          Regular (Just "parameters" :>: (Nothing :@: parameters))]

data AlgorithmIdentifier =
   AlgorithmIdentifier {
      algorithm1  :: Algorithm,
      parameters1 :: Maybe Parameters } 
   deriving (Eq,Show)

instance Encode AlgorithmIdentifier where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            AlgorithmIdentifier {
               algorithm1  = fromJust $ decode (as!!0) (bs!!0),
               parameters1 = Nothing
            }


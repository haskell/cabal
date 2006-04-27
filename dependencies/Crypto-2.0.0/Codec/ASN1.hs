-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.ASN1
-- Copyright   :  (c) Dominic Steinitz 2005
-- License     :  BSD-style (see the file ReadMe.tex)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Typecheck and decode an abstract BER representations (as, for
-- example, produced by Codec.ASN1.TLV).
--
-----------------------------------------------------------------------------

module Codec.ASN1 (
   -- * Types
   TagType(..),
   TagValue,
   TagPlicity(..),
   AbsPrimType(..),
   AbstractType(..),
   TypeDefn(..),
   TaggedType(..),
   NamedType(..),
   ComponentType(..),
   -- * Haskell Equivalences of Base ASN.1 Types
   VisibleString(..),
   PrintableString(..),
   IA5String(..),
   DirectoryString(..),
   OID(..),
   NULL,
   OctetString(..),
   BitString(..),
   SetOf(..),
   -- * Modifiers and Accessor Functions
   modName,
   getAbsType,
   modTagType,
   modTagVal,
   absRefedType,
   absSeqComponents,
   absSeqOfType,
   absSetOfType,
   unVisibleString,
   unDirectoryString,
   unSetOf,
   -- * Base ASN.1 Type Definitions
   absVisibleString,
   absPrintableString,
   absIA5String,
   absInteger,
   absOID,
   absNull,
   absOctetString,
   absBitString,
   -- * Auxilliary ASN.1 Type Definitions
   commonName,
   organizationUnitName,
   emailAddress,
   domainComponent,
   -- * Association Table of Types and OIDs
   oids
      ) where

import Char
import Data.FiniteMap
import Codec.Utils

data TagType = Universal | Application | Context | Private
   deriving (Eq,Show, Enum)

type TagValue = Integer

data TagPlicity = Implicit | Explicit
   deriving (Eq,Show)

data AbsPrimType = AbsVisibleString
                 | AbsPrintableString
                 | AbsIA5String
                 | AbsBool
                 | AbsInteger
                 | AbsOID 
                 | AbsNull
                 | AbsOctetString
                 | AbsBitString
   deriving (Eq,Show)

data AbstractType = AbsBasePrim TagType TagValue AbsPrimType
                  | AbsRef TagType TagValue TagPlicity TypeDefn
                  | AbsSeq TagType TagValue TagPlicity [ComponentType]
                  | AbsSeqOf TagType TagValue TagPlicity TypeDefn
                  | AbsSetOf TagType TagValue TagPlicity TypeDefn
                  | AbsAnyDefBy ComponentIndex
   deriving (Eq,Show)

data TaggedType = Maybe TagValue :@: TypeDefn
  deriving (Eq,Show)

data NamedType = Maybe String :>: TaggedType
   deriving (Eq,Show)

{-
For now. We should probably replace [Octet] by an existential type
and know how to encode it rather than forcing the user to encode it
by hand for a specific encoding.
-}

{-
Also for now, we will hand code the Component Index.
-}

type ComponentIndex = Int

data ComponentType = Regular NamedType
                   | Optional NamedType
                   | Default NamedType [Octet]
                   | AnyDefBy ComponentIndex
   deriving (Eq,Show)

data TypeDefn = String ::= AbstractType
   deriving (Eq,Show)

class Tagged a where
   modTagVal :: Maybe TagValue -> a -> a
   modTagType :: TagType -> a -> a 

instance Tagged AbstractType where
   modTagVal x a@(AbsBasePrim tt tv at) = 
      case x of 
         Nothing -> a
         Just y -> AbsBasePrim tt y at
   modTagVal x a@(AbsRef tt tv tp at) =
      case x of
         Nothing -> a
         Just y -> AbsRef tt y tp at
   modTagVal x a@(AbsSeq tt tv tp as) = 
      case x of
         Nothing -> a
         Just y -> AbsSeq tt y tp as
   modTagVal x a@(AbsSeqOf tt y tp td) =
      case x of
         Nothing -> a
         Just y -> AbsSeqOf tt y tp td
   modTagType x a@(AbsBasePrim tt tv at) = 
      AbsBasePrim x tv at
   modTagType x a@(AbsRef tt tv tp at) =
      AbsRef x tv tp at
   modTagType x a@(AbsSeq tt tv tp as) = 
      AbsSeq x tv tp as
   modTagType x a@(AbsSeqOf tt tv tp td) =
      AbsSeqOf x tv tp td

instance Tagged TypeDefn where
   modTagVal x (n ::= t) = n ::= (modTagVal x t)
   modTagType x (n ::= t) = n ::= (modTagType x t)

-- | Create a new type definition from an existing one.

modName :: String -> TypeDefn -> TypeDefn
modName x (_ ::= at) = (x ::= at)

getAbsType (_ ::= t) = t

-- | Get the components of a SEQUENCE.

-- absSeqComponents :: AbstractType -> [AbstractType]
absSeqComponents (AbsSeq _ _ _ as) = ats
   where ats = map f as
         f (Regular  (_ :>: (_ :@: (_ ::= x)))) = x
         f (Optional (_ :>: (_ :@: (_ ::= x)))) = x
         f (Default  (_ :>: (_ :@: (_ ::= x))) _) = x
         f (AnyDefBy n) = AbsAnyDefBy n

-- | Get the component of the SEQUENCE OF.

absSeqOfType :: AbstractType -> AbstractType
absSeqOfType (AbsSeqOf _ _ _ (_ ::= x)) = x

-- | Get the component of the SET OF.

absSetOfType :: AbstractType -> AbstractType
absSetOfType (AbsSetOf _ _ _ (_ ::= x)) = x

-- | Get the component of a referenced type.

absRefedType :: AbstractType -> AbstractType
absRefedType (AbsRef _ _ _ (_ ::= x)) = x

data VisibleString = VisibleString String
   deriving (Eq,Show)

unVisibleString :: VisibleString -> String
unVisibleString (VisibleString x) = x

data PrintableString = PrintableString String
   deriving (Eq,Show)

data IA5String = IA5String String
   deriving (Eq,Show)

data DirectoryString = VS VisibleString 
                     | PS PrintableString
                     | IA IA5String
   deriving (Eq,Show)

unDirectoryString (PS (PrintableString x)) = x
unDirectoryString (VS (VisibleString x)) = x
unDirectoryString (IA (IA5String x)) = x

newtype OID = OID [Integer]
   deriving (Eq, Show, Ord)

data NULL = NULL
   deriving (Eq, Show)

data OctetString = OctetString [Octet]
   deriving (Eq,Show)

data BitString = BitString [Octet]
   deriving (Eq,Show)

data SetOf a = SetOf [a]
   deriving (Eq,Show)

unSetOf (SetOf x) = x

absVisibleString :: TypeDefn
absVisibleString = 
   "VisibleString" ::= AbsBasePrim Universal 26 AbsVisibleString

absPrintableString :: TypeDefn
absPrintableString =
   "PrintableString" ::= AbsBasePrim Universal 19 AbsPrintableString

absIA5String :: TypeDefn
absIA5String =
   "IA5String" ::= AbsBasePrim Universal 22 AbsIA5String

absInteger :: TypeDefn
absInteger =
   "Integer" ::= AbsBasePrim Universal 2 AbsInteger

absOID :: TypeDefn
absOID =
   "OID" ::= AbsBasePrim Universal 6 AbsOID

absNull :: TypeDefn
absNull =
   "NULL" ::= AbsBasePrim Universal 5 AbsNull

absOctetString :: TypeDefn
absOctetString =
   "OCTET STRING" ::= AbsBasePrim Universal 4 AbsOctetString

absBitString :: TypeDefn
absBitString =
   "BIT STRING" ::= AbsBasePrim Universal 3 AbsBitString

commonName :: TypeDefn
commonName = modName "CommonName" absPrintableString

countryName :: TypeDefn
countryName = modName "CountryName" absPrintableString

localityName :: TypeDefn
localityName = modName "LocalityName" absPrintableString

organization :: TypeDefn
organization = modName "Organization" absPrintableString

organizationUnitName :: TypeDefn
organizationUnitName = modName "OrganizationUnitName" absPrintableString

emailAddress :: TypeDefn
emailAddress = modName "EmailAddress" absIA5String

domainComponent :: TypeDefn
domainComponent = modName "DomainComponent" absIA5String

oids :: FiniteMap OID TypeDefn

oids = 
   addListToFM emptyFM [
       (OID [2,5,4,3],commonName),
       (OID [2,5,4,6],countryName),
       (OID [2,5,4,7],localityName),
       (OID [2,5,4,10],organization),
       (OID [2,5,4,11],organizationUnitName),
       (OID [1,2,840,113549,1,9,1],emailAddress),
       (OID [0,9,2342,19200300,100,1,25],domainComponent)
   ]

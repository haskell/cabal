module Main(main) where

import Data.Char
import Data.Maybe
import Control.Monad.Error
import Codec.ASN1.BER
import Codec.ASN1
import Test.HUnit

{-
Some of the ASN.1 definitions are taken from various standards and
these are annotated with references. The other ASN.1 definitions
have been created specifically to check decoding. These have been
checked using the on-line tool, Asnp, available at
 
http://asn1.elibel.tm.fr/en/tools/asnp/index.htm

Notes: Definitions using ANY DEFINED BY have to be checked with -1990
option. Asnp was developed in Objective Caml.
-}

expectSuccess testName asnType berValue expectedAbsValue =
   TestCase $
      do w <- tc asnType berValue
         let x = decode (getAbsType asnType) (Just w)
         let (Just y) = x
         assertEqual testName expectedAbsValue y

expectFailure testName asnType berValue expectedError = 
   TestCase $
      do x <- (do y <- tc asnType berValue
                  return "Unexpected successful typechecking") 
              `catchError` (\e -> return $ show e)
         assertEqual testName x expectedError

{-
Some tagged value tests. See 8.14.3 of X.690 (ISO 8825-1).

Type1 ::= VisibleString
Type2 ::= [APPLICATION 3] IMPLICIT Type1
Type3 ::= [2] Type2
Type4 ::= [APPLICATION 7] IMPLICIT Type3
Type5 ::= [2] IMPLICIT Type2
-}

type1' = modName "Type1" absVisibleString

type Type1 = VisibleString

jones1 = Primitive Universal 26 5 [0x4a,0x6f,0x6e,0x65,0x73]

decodedJones1 = VisibleString "Jones"

tagTest1 = expectSuccess "Type1" type1' jones1 decodedJones1

type2  = "Type2" ::= AbsRef Application 3 Implicit type1'

data Type2 = Type2 VisibleString
   deriving (Eq,Show)

instance Encode Type2 where
   decode a b = 
      do let (AbsRef _ _ _ a') = a 
             a'' = getAbsType a'
         x <- decode a'' b
         return $ Type2 x

jones2 = Primitive Application 3 5 [0x4a,0x6f,0x6e,0x65,0x73]

decodedJones2 = Type2 decodedJones1

tagTest2 = expectSuccess "Type2" type2 jones2 decodedJones2

type3  = "Type3" ::= AbsRef Context 2 Explicit type2

data Type3 = Type3 Type2
   deriving (Eq,Show)

instance Encode Type3 where
   decode a b = 
      do let (AbsRef _ _ _ a') = a
             a'' = getAbsType a'
         x <- decode a'' b
         return $ Type3 x

jones3 = Constructed Context 2 7 [jones2]

decodedJones3 = Type3 decodedJones2

tagTest3 = expectSuccess "Type3" type3 jones3 decodedJones3

type4  = "Type4" ::= AbsRef Application 7 Implicit type3

jones4 = Constructed Application 7 7 [jones2]

data Type4 = Type4 Type3
   deriving (Eq,Show)

instance Encode Type4 where
   decode a b = 
      do let (AbsRef _ _ _ a') = a
             a'' = getAbsType a'
         x <- decode a'' b
         return $ Type4 x   

decodedJones4 = Type4 decodedJones3

tagTest4 = expectSuccess "Type4" type4 jones4 decodedJones4

{-
Some tests for OPTIONAL components.
-}

{-
Journey ::= 
   SEQUENCE {
      origin IA5String,
      stop1 [0] IA5String  OPTIONAL,
      stop2 [1] IA5String  OPTIONAL,
      destination IA5String
   }
-}

journey =
   "Journey" ::=
      AbsSeq Universal 16 Implicit [
         Regular  (Just "origin"       :>: (Nothing  :@: absIA5String)),
         Optional (Just "stop1"        :>: (Just 0   :@: absIA5String)),
         Optional (Just "stop2"        :>: (Just 1   :@: absIA5String)),
         Regular  (Just "destination"  :>: (Nothing  :@: absIA5String))
      ]

j1 = 
   Constructed Universal 16 24 [
      Primitive Universal 22 3 [97,97,98],
      Constructed Context 0 5 [Primitive Universal 22 3 [99,100,101]],
      Constructed Context 1 5 [Primitive Universal 22 3 [102,103,104]],     
      Primitive Universal 22 3 [97,97,98]
   ]

j2 = 
   Constructed Universal 16 24 [
      Primitive Universal 22 3 [97,97,98],
      Constructed Context 1 5 [Primitive Universal 22 3 [102,103,104]],     
      Primitive Universal 22 3 [97,97,98]
   ]

data Journey =
   Journey {
      origin :: IA5String,
      stop1 :: Maybe IA5String,
      stop2 :: Maybe IA5String,
      destination :: IA5String
   }
   deriving (Eq,Show)

instance Encode Journey where
   decode a b =
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            Journey {
               origin      = fromJust (decode (as'!!0) (bs!!0)),
               stop1       = decode (as'!!1) (bs!!1),
               stop2       = decode (as'!!2) (bs!!2),
               destination = fromJust (decode (as'!!3) (bs!!3))
            }

decodedJ1 =
   Journey {
      origin = IA5String "aab",
      stop1 = Just $ IA5String "cde",
      stop2 = Just $ IA5String "fgh",
      destination = IA5String "aab"
   }
 
decodedJ2 =
   Journey {
      origin = IA5String "aab",
      stop1 = Nothing,
      stop2 = Just $ IA5String "fgh",
      destination = IA5String "aab"
   }

journeyTest1 =
   expectSuccess "Journey1" journey j1 decodedJ1 

journeyTest2 =
   expectSuccess "Journey2" journey j2 decodedJ2 

{-
Odyssey ::= SEQUENCE {
   start Journey,
   trip1 [0] Journey OPTIONAL,
   trip2 [1] Journey OPTIONAL,
   trip3 [2] Journey OPTIONAL,
   end Journey
   }
-}

odyssey =
   "Odyssey" ::=
      AbsSeq Universal 16 Implicit [
         Regular  (Just "start"       :>: (Nothing  :@: journey)),
         Optional (Just "trip1"       :>: (Just 0   :@: journey)),
         Optional (Just "trip2"       :>: (Just 1   :@: journey)),
         Optional (Just "trip3"       :>: (Just 2   :@: journey)),
         Regular  (Just "end"         :>: (Nothing  :@: journey))
      ]

o1 = 
   Constructed Universal 16 130 [
      j1,
      Constructed Context 0 26 [j1],
      Constructed Context 1 26 [j1],     
      Constructed Context 2 26 [j1],     
      j1
   ]

o2 = 
   Constructed Universal 16 52 [
      j1,
      j1
   ]

data Odyssey =
   Odyssey {
      start :: Journey,
      trip1 :: Maybe Journey,
      trip2 :: Maybe Journey,
      trip3 :: Maybe Journey,
      end   :: Journey
   }
   deriving (Eq,Show)

instance Encode Odyssey where
   decode a b =
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            Odyssey {
               start  = fromJust (decode (as'!!0) (bs!!0)),
               trip1  = decode (as'!!1) (bs!!1),
               trip2  = decode (as'!!2) (bs!!2),
               trip3  = decode (as'!!2) (bs!!2),
               end    = fromJust (decode (as'!!4) (bs!!4))
            }

decodedO1 =
   Odyssey {
      start = decodedJ1,
      trip1 = Just decodedJ1,
      trip2 = Just decodedJ1,
      trip3 = Just decodedJ1,
      end = decodedJ1
   } 

decodedO2 =
   Odyssey {
      start = decodedJ1,
      trip1 = Nothing,
      trip2 = Nothing,
      trip3 = Nothing,
      end = decodedJ1
   } 

odysseyTest1 =
   expectSuccess "Odyssey1" odyssey o1 decodedO1 

odysseyTest2 =
   expectSuccess "Odyssey2" odyssey o2 decodedO2 

{-
FunnyOptional ::= 
   SEQUENCE {
      perhaps [0] IA5String OPTIONAL
   }
-}

funnyOptional =
   "FunnyOptional" ::=
      AbsSeq Universal 16 Implicit [
         Optional (Just "perhaps" :>: (Just 0   :@: absIA5String))
      ]

fo1 = 
   Constructed Universal 16 7 [
      Constructed Context 0 5 [
         Primitive Universal 22 3 [97,97,98]
      ]
   ]

fo2 = Constructed Universal 16 0 []

data FunnyOptional =
   FunnyOptional {
      perhaps :: Maybe IA5String
   }
   deriving (Eq,Show)

instance Encode FunnyOptional where
   decode a b =
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            FunnyOptional {
               perhaps = decode (as'!!0) (bs!!0)
            }

decodedFO1 =
   FunnyOptional {
      perhaps = Just $ IA5String "aab"
   }

funnyOptionalTest1 =
   expectSuccess "FunnyOptional1" funnyOptional fo1 decodedFO1 

decodedFO2 =
   FunnyOptional {
      perhaps = Nothing
   }

funnyOptionalTest2 =
   expectSuccess "FunnyOptional2" funnyOptional fo2 decodedFO2 


{-
Some ANY DEFINED BY tests. See the former versions of the ASN.1
standards, X.208 and X.209, sometimes referred to as ASN.1:1988 or
ASN.1:1990. This was used in some definitions of X.509 certificates,
for example:

AlgorithmIdentifier  ::=  SEQUENCE  {
     algorithm               OBJECT IDENTIFIER,
     parameters              ANY DEFINED BY algorithm OPTIONAL  }
                                -- contains a value of the type
                                -- registered for use with the
                                -- algorithm object identifier value
-}

{-
TextBook  =  SEQUENCE
    {
      author          PrintableString,
      citationType    OID,
      reference       ANY DEFINED BY CitationType
    }
-}

textBook =
   "TextBook" ::= 
      AbsSeq Universal 16 Implicit 
         [Regular (Just "author" :>: (Nothing :@: absPrintableString)),
          Regular (Just "citationType"  :>: (Nothing :@: absOID)),
          AnyDefBy 1]

data TextBook =
   TextBook {
      author        :: PrintableString,
      citationType  :: OID,
      reference     :: PrintableString
   }
   deriving (Eq,Show)

instance Encode TextBook where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            TextBook {
               author       = let (Just x) = decode (as'!!0) (bs!!0) in x,
               citationType = let (Just x) = decode (as'!!1) (bs!!1) in x,
               reference    = let (Just x) = decode (as'!!2) (bs!!2) in x
            }

encodedPrintableString1 = 
   Primitive Universal 19 5 [104,101,108,108,111]

encodedPrintableString2 = 
   Primitive Universal 19 5 [105,101,108,108,111]

encodedPrintableString3 = 
   Primitive Universal 19 5 [106,101,108,108,111]

encodedPrintableString4 = 
   Primitive Universal 19 5 [107,101,108,108,111]

encodedOID1 = Primitive Universal 6 3 [85,4,7]

encodedTextBook1 = 
   Constructed Universal 16 13 [
      encodedPrintableString1,
      encodedOID1
   ]

decodedTextBook1 = 
   "user error (Checking AnyDefBy 1: insufficient components)"

encodedTextBook2 = 
   Constructed Universal 16 13 [
      encodedPrintableString1,
      encodedOID1,
      encodedPrintableString2
   ]

decodedTextBook2 =
   TextBook {
      author = PrintableString "hello",
      citationType = OID [2,5,4,7],
      reference = PrintableString "iello"
   }

encodedTextBook3 = 
   Constructed Universal 16 13 [
      encodedPrintableString3,
      encodedOID1,
      encodedPrintableString4
   ]

decodedTextBook3 =
   TextBook {
      author = PrintableString "jello",
      citationType = OID [2,5,4,7],
      reference = PrintableString "kello"
   }

textBookTest1 =
   expectFailure "TextBook1" textBook encodedTextBook1 decodedTextBook1

textBookTest2 = 
   expectSuccess "TextBook2" textBook encodedTextBook2 decodedTextBook2

textBookTest3 = 
   expectSuccess "TextBook3" textBook encodedTextBook3 decodedTextBook3

library =
   "Library" ::=
      AbsSeq Universal 16 Implicit
         [Regular (Just "first" :>: (Nothing :@: textBook)),
          Regular (Just "second" :>: (Nothing :@: textBook))]

data Library =
   Library {
      first :: TextBook,
      second :: TextBook
   }
   deriving (Eq,Show)

instance Encode Library where
   decode a b = 
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            Library {
               first = let (Just x) = decode (as'!!0) (bs!!0) in x,
               second = let (Just x) = decode (as'!!1) (bs!!1) in x
            }

encodedLibrary =
   Constructed Universal 16 28 [encodedTextBook2,encodedTextBook3]

decodedLibrary =
   Library {
      first = decodedTextBook2,
      second = decodedTextBook3
   }

libraryTest = 
   expectSuccess "Library1" library encodedLibrary decodedLibrary

v1 = Primitive Universal 26 5 [104,101,108,108,111] -- Valid
v2 = Primitive Universal 26 5 [103,101,108,108,111] -- Valid
v3 = Primitive Universal 26 5 [31,101,108,108,111]  -- Not valid VisibleString

expectedv1 = VisibleString "hello"
expectedv2 = VisibleString "gello"
expectedv3 = 
   "user error (Checking \"VisibleString\": type not compatible " ++
   "with values [31,101,108,108,111])"

visibleStringTest1 = 
   expectSuccess "VisibleString1" absVisibleString v1 expectedv1

visibleStringTest2 = 
   expectSuccess "VisibleString2" absVisibleString v2 expectedv2

visibleStringTest3 = 
   expectFailure "VisibleString3" absVisibleString v3 expectedv3

{-
A modified version of the example in Annex A of X.690 (ISO 8825-1).
-}

{-
Name ::= [APPLICATION 1] IMPLICIT SEQUENCE
   {givenName  VisibleString,
    initial    VisibleString,
    familyName VisibleString}
-}

name = 
   "Name" ::= 
      AbsSeq Application 1 Implicit [
         Regular (Just "givenName"  :>: (Nothing :@: absVisibleString)),
         Regular (Just "initial"    :>: (Nothing :@: absVisibleString)),
         Regular (Just "familyName" :>: (Nothing :@: absVisibleString))
      ]

data Name = Name {givenName  :: VisibleString,
                  initial    :: VisibleString,
                  familyName :: VisibleString}
   deriving (Eq,Show)

instance Encode Name where
   decode a b =
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            Name {
               givenName  = let (Just x) = decode (as'!!0) (bs!!0) in x,
               initial    = let (Just x) = decode (as'!!1) (bs!!1) in x,
               familyName = let (Just x) = decode (as'!!2) (bs!!2) in x
            }

n1 = Constructed Application 1 14 [v1,v2]           -- Invalid number
                                                    -- of components

n2 = Constructed Application 1 14 [v1]              -- Invalid number
                                                    -- of components

n3 = Constructed Application 1 14 []                -- Invalid number
                                                    -- of components  

n4 = Constructed Application 1 14 [v1,v2,v1]        -- Valid

n5 = Constructed Application 1 14 [v1,v2,v3]        -- Invalid component

expectedn1 = 
   "user error (Checking Regular (Just \"familyName\" :>: " ++
       "(Nothing :@: (\"VisibleString\" ::= " ++
           "AbsBasePrim Universal 26 AbsVisibleString))): " ++
       "insufficient components)"

nameTest1 =
   expectFailure "Name1" name n1 expectedn1 

expectedn2 = 
   "user error (Checking Regular (Just \"initial\" :>: " ++
       "(Nothing :@: (\"VisibleString\" ::= " ++
           "AbsBasePrim Universal 26 AbsVisibleString))): " ++
       "insufficient components)"

nameTest2 = 
   expectFailure "Name2" name n2 expectedn2 

expectedn3 = 
   "user error (Checking Regular (Just \"givenName\" :>: " ++
       "(Nothing :@: (\"VisibleString\" ::= " ++
           "AbsBasePrim Universal 26 AbsVisibleString))): " ++
       "insufficient components)"

nameTest3 = 
   expectFailure "Name3" name n3 expectedn3 

expectedn4 =
   Name {
      givenName = VisibleString "hello",
      initial   = VisibleString "gello",
      familyName = VisibleString "hello"
   }
      
nameTest4 =
   expectSuccess "Name4" name n4 expectedn4 

expectedn5 =
   "user error (Checking \"VisibleString\": " ++
   "type not compatible with values [31,101,108,108,111])"

nameTest5 = 
   expectFailure "Name5" name n5 expectedn5 

{-
EmployeeNumber ::= [APPLICATION 2] IMPLICIT INTEGER
-}

employeeNumber =
   "EmployeeNumber" ::= AbsRef Application 2 Implicit absInteger

data EmployeeNumber = EmployeeNumber Integer
   deriving (Eq,Show)

instance Encode EmployeeNumber where
   decode a b = 
      do let (AbsRef _ _ _ a') = a 
             a'' = getAbsType a'
         x <- decode a'' b
         return $ EmployeeNumber x

en1 = Primitive Application 2 1 [0x33]

decodedEN1 = EmployeeNumber 51

enTest1 =
   expectSuccess "EmployeeNumber1" employeeNumber en1 decodedEN1 

{-
Date ::= [APPLICATION 3] IMPLICIT VisibleString -- YYYYMMDD
-}

date = "Date" ::= 
          AbsRef Application 3 Implicit absVisibleString

data Date = Date VisibleString
   deriving (Eq,Show)

instance Encode Date where
   decode a b = 
      do let (AbsRef _ _ _ a') = a 
             a'' = getAbsType a'
         x <- decode a'' b
         return $ Date x

b = "30/03/2003 19:37:34 GMT"
a = "30/03/2004 19:37:34 GMT"

nb = map (fromIntegral . ord) b
na = map (fromIntegral . ord) a

d1 = Constructed Application 3 7 [Primitive Universal 23 23 na] -- Invalid
d2 = Primitive Application 3 6 nb                               -- Valid

expectedD1 = 
   "user error (Checking \"Date\": " ++ 
   "expected PRIMITIVE Tag found CONSTRUCTED Tag)"
decodedD2 = Date $ VisibleString b

dateTest1 = 
   expectFailure "Date1" date d1 expectedD1 

dateTest2 =
   expectSuccess "Date2" date d2 decodedD2

{-
ChildInformation ::= SEQUENCE
    { name        Name,
      dateOfBirth [0] Date}
-}

childInformation = 
   "ChildInformation" ::= 
      AbsSeq Universal 16 Implicit [
         Regular (Just "name"        :>: (Nothing :@: name)),
         Regular (Just "dateOfBirth" :>: (Just 0 :@: date))
      ]

data ChildInformation = 
   ChildInformation { name1 :: Name,
                      dateOfBirth :: Date }
   deriving (Eq,Show)

instance Encode ChildInformation where
   decode a b =
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            ChildInformation {
               name1       = fromJust $ decode (as'!!0) (bs!!0),
               dateOfBirth = fromJust $ decode (as'!!1) (bs!!1)
            }

ci1 = Constructed Universal 16 28
         [n4,Constructed Context 0 25 [d2]] 

expectedCI1 =
   ChildInformation {
      name1 = expectedn4,
      dateOfBirth = decodedD2
   }

ciTest1 =
   expectSuccess "ChildInformation1" childInformation ci1 expectedCI1

{-
PersonnelRecord ::= [APPLICATION 0] IMPLICIT SEQUENCE {
   name         Name,
   title        [0] VisibleString,
   number       EmployeeNumber,
   dateOfHire   [1] Date,
   nameOfSpouse [2] Name,
   children     [3] IMPLICIT
      SEQUENCE OF ChildInformation DEFAULT {} }
-}

personnelRecord =
   "PersonnelRecord" ::=
      AbsSeq Application 0 Implicit [
         Regular (Just "name"         :>: (Nothing  :@: name)),
         Regular (Just "title"        :>: (Just 0   :@: absVisibleString)),
         Regular (Just "number"       :>: (Nothing  :@: employeeNumber)),
         Regular (Just "dateOfHire"   :>: (Just 1   :@: date)),
         Regular (Just "nameOfSpouse" :>: (Just 2   :@: name)),
         Regular (
            Just "children"     :>: (
               Just 3   :@: (
                  "SEQUENCE OF ChildInformation" ::= 
                     AbsSeqOf Universal 16 Implicit childInformation
               )
            )
         )
      ]

data PersonnelRecord = 
   PersonnelRecord {name2 :: Name,
                    title :: VisibleString,
                    number :: EmployeeNumber,
                    dateOfHire :: Date,
                    nameOfSpouse :: Name,
                    children :: [ChildInformation]}
   deriving (Eq,Show)

instance Encode PersonnelRecord where
   decode a b =
      do x <- b
         let as = absSeqComponents a
             bs  = encodedDefComps x
             as' = map (\x -> replaceRef x as bs) as
         return $ 
            PersonnelRecord {
               name2 = fromJust $ decode (as'!!0) (bs!!0),
               title = fromJust $ decode (as'!!1) (bs!!1),
               number = fromJust $ decode (as'!!2) (bs!!2),
               dateOfHire = fromJust $ decode (as'!!3) (bs!!3),
               nameOfSpouse = fromJust $ decode (as'!!4) (bs!!4),
               children = fromJust $ decode (as'!!5) (bs!!5)}

pr1 = 
   Constructed Application 0 23 [
      n4,
      Constructed Context 0 7 [v1],
      en1,
      Constructed Context 1 8 [d2],
      Constructed Context 2 7 [n4],
      Constructed Context 3 18 [Constructed Universal 16 16 [ci1]]
   ]

decodedpr1 =
   PersonnelRecord {
      name2 = expectedn4,
      title = expectedv1,
      number = decodedEN1,
      dateOfHire = decodedD2,
      nameOfSpouse = expectedn4,
      children = [expectedCI1]
   }

prTest1 =
   expectSuccess "PersonnelRecord1" personnelRecord pr1 decodedpr1 

tests = 
   TestList [
      tagTest1, tagTest2, tagTest3, tagTest4, 
      textBookTest1, textBookTest2, textBookTest3, libraryTest, 
      visibleStringTest1, visibleStringTest2, visibleStringTest3, nameTest1,
      nameTest2, nameTest3, nameTest4, nameTest5,
      enTest1, dateTest1, dateTest2, ciTest1,
      prTest1, journeyTest1, journeyTest2, odysseyTest1,
      odysseyTest2, funnyOptionalTest1, funnyOptionalTest2
   ]

main = runTestTT tests

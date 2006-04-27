{- |
   This module defines an internal (generic) representation for XML
   documents including their DTDs.

   History:
   The original module was derived by hand from the XML specification,
   following the grammar precisely.  Then we simplified the types,
   removing layers of indirection and redundancy, and generally making
   things easier to work with.  Then we allowed PEReferences to be
   ubiquitous, by removing them from the types and resolving all
   PE references at parse-time.  Finally, we added a per-document
   symbol table for GEReferences, and a whitespace-significance flag
   for plaintext.
-}

module Text.XML.HaXml.Types
  (
  -- * A simple symbol table mapping strings (references) to values.
    SymTab
  -- ** Symbol table operations
  , emptyST
  , addST
  , lookupST

  -- * XML Types
  -- ** The top-level document container
  , Document(..)

  -- ** The main document content
  , Element(..)
  , ElemTag(..)
  , Content(..)
  , Attribute
  , AttValue(..)

  -- ** Administrative parts of the document
  , Prolog(..)
  , XMLDecl(..)
  , Misc(..)
  , ProcessingInstruction
  , SDDecl
  , VersionInfo
  , Comment
  , PITarget

  -- ** The DTD
  -- *** content model
  , DocTypeDecl(..)
  , MarkupDecl(..)
  , ExtSubset(..)
  , ExtSubsetDecl(..)
  , ElementDecl(..)
  , ContentSpec(..)
  , CP(..)
  , Modifier(..)
  , Mixed(..)

  -- *** attribute model
  , AttListDecl(..)
  , AttDef(..)
  , AttType(..)
  , TokenizedType(..)
  , EnumeratedType(..)
  , NotationType
  , Enumeration
  , DefaultDecl(..)
  , FIXED(..)

  -- *** conditional sections
  , ConditionalSect(..)
  , IncludeSect
  , IgnoreSect
  , Ignore(..)
  , IgnoreSectContents(..)

  -- ** References
  , Reference(..)
  , EntityRef
  , CharRef
  , PEReference

  -- ** Entities
  , EntityDecl(..)
  , GEDecl(..)
  , PEDecl(..)
  , EntityDef(..)
  , PEDef(..)
  , ExternalID(..)
  , NDataDecl(..)
  , TextDecl(..)
  , ExtParsedEnt(..)
  , ExtPE(..)
  , NotationDecl(..)
  , PublicID(..)
  , EncodingDecl(..)
  , EntityValue(..)
  , EV(..)
  , PubidLiteral(..)
  , SystemLiteral(..)

  -- ** Basic value types
  , Name
  , Names
  , NmToken
  , NmTokens
  , CharData
  , CDSect
  ) where



{- A simple symbol table for storing macros whilst parsing. -}

type SymTab a = [(String,a)]

emptyST :: SymTab a
emptyST  = []

addST :: String -> a -> SymTab a -> SymTab a
addST n v = ((n,v):)

lookupST :: String -> SymTab a -> Maybe a
lookupST = lookup



{- XML types start here -}

-- | The symbol table stored in a document holds all its general entity
--   reference definitions.
data Document = Document Prolog (SymTab EntityDef) Element
data Prolog   = Prolog (Maybe XMLDecl) (Maybe DocTypeDecl)
data XMLDecl  = XMLDecl VersionInfo (Maybe EncodingDecl) (Maybe SDDecl) 
data Misc     = Comment Comment
              | PI ProcessingInstruction
              
type ProcessingInstruction = (PITarget,String)

type SDDecl      = Bool 
type VersionInfo = String 
type Comment     = String 
type PITarget    = String 

data DocTypeDecl = DTD Name (Maybe ExternalID) [MarkupDecl] 
data MarkupDecl  = Element  ElementDecl
                 | AttList  AttListDecl
                 | Entity   EntityDecl
                 | Notation NotationDecl
                 | MarkupMisc Misc

data ExtSubset     = ExtSubset (Maybe TextDecl) [ExtSubsetDecl] 
data ExtSubsetDecl = ExtMarkupDecl MarkupDecl
                   | ExtConditionalSect ConditionalSect

data Element   = Elem Name [Attribute] [Content]
data ElemTag   = ElemTag Name [Attribute]	-- ^ intermediate for parsing
type Attribute = (Name, AttValue)
data Content   = CElem Element
               | CString Bool CharData
			-- ^ bool is whether whitespace is significant
               | CRef Reference
               | CMisc Misc

data ElementDecl = ElementDecl Name ContentSpec
data ContentSpec = EMPTY
                 | ANY
                 | Mixed Mixed
                 | ContentSpec CP
data CP = TagName Name Modifier
        | Choice [CP] Modifier
        | Seq [CP] Modifier 
data Modifier = None  -- ^ Just One
              | Query -- ^ Zero Or One
              | Star  -- ^ Zero Or More
              | Plus  -- ^ One Or More 
data Mixed = PCDATA
           | PCDATAplus [Name] 

data AttListDecl = AttListDecl Name [AttDef]
data AttDef      = AttDef Name AttType DefaultDecl 
data AttType     = StringType
                 | TokenizedType TokenizedType
                 | EnumeratedType EnumeratedType 
data TokenizedType = ID
                   | IDREF
                   | IDREFS
                   | ENTITY
                   | ENTITIES
                   | NMTOKEN
                   | NMTOKENS 
data EnumeratedType = NotationType NotationType
                    | Enumeration Enumeration 
type NotationType   = [Name]	-- nonempty list
type Enumeration    = [NmToken]	-- nonempty list
data DefaultDecl    = REQUIRED
                    | IMPLIED
                    | DefaultTo AttValue (Maybe FIXED) 
data FIXED          = FIXED 

data ConditionalSect = IncludeSect IncludeSect
                     | IgnoreSect IgnoreSect 
type IncludeSect = [ExtSubsetDecl]
type IgnoreSect  = [IgnoreSectContents]
data Ignore      = Ignore
data IgnoreSectContents = IgnoreSectContents Ignore [(IgnoreSectContents,Ignore)] 

data Reference    = RefEntity EntityRef
                  | RefChar CharRef 
                  deriving Eq
type EntityRef    = Name 
type CharRef      = Int
type PEReference  = Name 

data EntityDecl   = EntityGEDecl GEDecl
                  | EntityPEDecl PEDecl 
data GEDecl       = GEDecl Name EntityDef 
data PEDecl       = PEDecl Name PEDef 
data EntityDef    = DefEntityValue EntityValue
                  | DefExternalID ExternalID (Maybe NDataDecl) 
data PEDef        = PEDefEntityValue EntityValue
                  | PEDefExternalID ExternalID 
data ExternalID   = SYSTEM SystemLiteral
                  | PUBLIC PubidLiteral SystemLiteral 
newtype NDataDecl = NDATA Name  

data TextDecl     = TextDecl (Maybe VersionInfo) EncodingDecl 
data ExtParsedEnt = ExtParsedEnt (Maybe TextDecl) Content 
data ExtPE        = ExtPE (Maybe TextDecl) [ExtSubsetDecl]

data NotationDecl    = NOTATION Name (Either ExternalID PublicID) 
newtype PublicID     = PUBLICID PubidLiteral 
newtype EncodingDecl = EncodingDecl String 

type Name     = String		 -- non-empty string
type Names    = [Name]		 -- non-empty list
type NmToken  = String		 -- non-empty string
type NmTokens = [NmToken]	 -- non-empty list

data AttValue    = AttValue [Either String Reference]
                 deriving Eq
data EntityValue = EntityValue [EV] 
data EV = EVString String
 --  -- | EVPERef PEReference
        | EVRef Reference 
newtype PubidLiteral  = PubidLiteral String 
newtype SystemLiteral = SystemLiteral String 
type CharData         = String 
type CDSect           = CharData

instance Eq ElemTag where
    (ElemTag n _) == (ElemTag m _)  = n==m

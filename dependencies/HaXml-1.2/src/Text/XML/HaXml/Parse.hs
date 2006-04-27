-- | A non-validating XML parser.  For the input grammar, see
--   <http://www.w3.org/TR/REC-xml>.
module Text.XML.HaXml.Parse
  (
  -- * Parse a whole document
    xmlParse, xmlParse'
  -- * Parse just a DTD
  , dtdParse, dtdParse'
  ) where

-- An XML parser, written using a slightly extended version of the
-- Hutton/Meijer parser combinators.  The input is tokenised internally
-- by the lexer xmlLex.  Whilst parsing, we gather a symbol
-- table of entity references.  PERefs must be defined before use, so we
-- expand their uses as we encounter them, forcing the remainder of the
-- input to be re-lexed and re-parsed.  GERefs are simply stored for
-- later retrieval.

import Prelude hiding (either,maybe,sequence)
import qualified Prelude (either)
import Data.Maybe hiding (maybe)
import Data.List (intersperse)       -- debugging only
import Data.Char (isSpace,isDigit,isHexDigit)
import Control.Monad hiding (sequence)
import Numeric (readDec,readHex)

import Text.XML.HaXml.Types
import Text.XML.HaXml.Lex
import Text.ParserCombinators.HuttonMeijerWallace


#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ > 502 )
import System.IO.Unsafe (unsafePerformIO)
#elif defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import IOExts (unsafePerformIO)
#elif defined(__NHC__) && ( __NHC__ > 114 )
import System.IO.Unsafe (unsafePerformIO)
#elif defined(__NHC__)
import IOExtras (unsafePerformIO)
#elif defined(__HBC__)
import UnsafePerformIO
#endif

--  #define DEBUG

#if defined(DEBUG)
#  if ( defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ > 502 ) || \
      ( defined(__NHC__) && __NHC__ > 114 )
import Debug.Trace(trace)
#  elif defined(__GLASGOW_HASKELL__) || defined(__HUGS__)
import IOExts(trace)
#  elif defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#  endif
debug :: a -> String -> a
v `debug` s = trace s v
#else
v `debug` s = v
#endif


-- | To parse a whole document, @xmlParse file content@ takes a filename
--   (for generating error reports) and the string content of that file.
--   A parse error causes program failure, with message to stderr.
xmlParse :: String -> String -> Document

-- | To parse a whole document, @xmlParse' file content@ takes a filename
--   (for generating error reports) and the string content of that file.
--   Any parse error message is passed back to the caller through the
--   @Either@ type.
xmlParse' :: String -> String -> Either String Document

-- | To parse just a DTD, @dtdParse file content@ takes a filename
--   (for generating error reports) and the string content of that
--   file.  If no DTD was found, you get @Nothing@ rather than an error.
--   However, if a DTD is found but contains errors, the program crashes.
dtdParse  :: String -> String -> Maybe DocTypeDecl

-- | To parse just a DTD, @dtdParse' file content@ takes a filename
--   (for generating error reports) and the string content of that
--   file.  If no DTD was found, you get @Right Nothing@.
--   If a DTD was found but contains errors, you get a @Left message@.
dtdParse' :: String -> String -> Either String (Maybe DocTypeDecl)

xmlParse  name  = Prelude.either error id . xmlParse' name
dtdParse  name  = Prelude.either error id . dtdParse' name

xmlParse' name  = sanitycheck . papply' (toEOF document) emptySTs . xmlLex name
dtdParse' name  = sanitycheck . papply' justDTD  emptySTs . xmlLex name

{-
sanitycheck :: Show p => [(a,s,[(p,t)])] -> a
sanitycheck [] = error "***Error at line 0: document not XML?"
sanitycheck ((x,_,[]):_) = x
sanitycheck ((x,_,s@((n,_):_)):xs) =
  x `debug` ("***Warning at "++show n++": data beyond end of parsed document")
-}

sanitycheck :: Show p => Either String [(a,s,[Either String (p,t)])]
                         -> Either String a
sanitycheck (Left err)          = Left err
sanitycheck (Right [])          = Left "***Error, no parse: document not XML?"
sanitycheck (Right ((x,_,_):_)) = Right x


---- Symbol table stuff ----

type SymTabs = (SymTab PEDef, SymTab EntityDef)

emptySTs :: SymTabs
emptySTs = (emptyST, emptyST)

addPE :: String -> PEDef -> SymTabs -> SymTabs
addPE n v (pe,ge) = (addST n v pe, ge)

addGE :: String -> EntityDef -> SymTabs -> SymTabs
addGE n v (pe,ge) = let newge = addST n v ge in newge `seq` (pe, newge)

lookupPE :: String -> SymTabs -> Maybe PEDef
lookupPE s (pe,ge) = lookupST s pe

flattenEV :: EntityValue -> String
flattenEV (EntityValue evs) = concatMap flatten evs
  where
    flatten (EVString s)          = s
    flatten (EVRef (RefEntity r)) = "&" ++r++";"
    flatten (EVRef (RefChar r))   = "&#"++show r++";"


---- Misc ----

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a


---- Auxiliary Parsing Functions ----
type XParser a = Parser SymTabs (Posn,TokenT) String a

name :: XParser Name
name = do {(p,TokName s) <- item; return s}

string, freetext :: XParser String
string   = do {(p,TokName s) <- item; return s}
freetext = do {(p,TokFreeText s) <- item; return s}

maybe :: XParser a -> XParser (Maybe a)
maybe p =
    ( p >>= return . Just) +++
    ( return Nothing)

either :: XParser a -> XParser b -> XParser (Either a b)
either p q =
    ( p >>= return . Left) +++
    ( q >>= return . Right)

word :: String -> XParser ()
word s = P (\st inp-> case inp of {
               (Left err: _) -> Left err;
               (Right (p,TokName n):ts) -> if s==n then Right [((),st,ts)]
                                                   else Right [];
               (Right (p,TokFreeText n):ts) -> if s==n then Right [((),st,ts)]
                                                       else Right [];
               ts -> Right [] } )

posn :: XParser Posn
posn = P (\st inp-> case inp of {
                     (Left err:_)    -> Left  err;
                     (Right (p,_):_) -> Right [(p,st,inp)];
                     []              -> Right [] } )

nmtoken :: XParser NmToken
nmtoken = (string +++ freetext)


peRef :: XParser a -> XParser a
peRef p =
    p +++
    ( do pn <- posn
         n <- pereference
         tr <- stquery (lookupPE n) `debug` ("Looking up %"++n)
         case tr of
           (Just (PEDefEntityValue ev)) ->
                      do reparse (xmlReLex (posInNewCxt ("macro %"++n++";")
                                                        (Just pn))
                                           (flattenEV ev))
                               `debug` ("  defn:  "++flattenEV ev)
                         peRef p
           (Just (PEDefExternalID (PUBLIC _ (SystemLiteral f)))) ->
                      do let val = unsafePerformIO (readFile f)
                         reparse (xmlReLex (posInNewCxt ("file "++f)
                                                        (Just pn)) val)
                               `debug` ("  reading from file "++f)
                         peRef p
           (Just (PEDefExternalID (SYSTEM (SystemLiteral f)))) ->
                      do let val = unsafePerformIO (readFile f)
                         reparse (xmlReLex (posInNewCxt ("file "++f)
                                                        (Just pn)) val)
                               `debug` ("  reading from file "++f)
                         peRef p
           Nothing -> mzero `elserror` "PEReference use before definition" )

blank :: XParser a -> XParser a
blank p =
    p +++
    ( do n <- pereference
         tr <- stquery (lookupPE n) `debug` ("Looking up %"++n++" (is blank?)")
         case tr of
           (Just (PEDefEntityValue ev))
                    | all isSpace (flattenEV ev)  ->
                            do blank p `debug` "Empty macro definition"
           (Just _) -> mzero
           Nothing  -> mzero `elserror` "PEReference use before definition" )



---- XML Parsing Functions ----

justDTD :: XParser (Maybe DocTypeDecl)
justDTD =
  ( do (ExtSubset _ ds) <- extsubset `debug` "Trying external subset"
       if null ds then mzero
         else return (Just (DTD "extsubset" Nothing (concatMap extract ds)))
  ) +++
  ( do (Prolog _ dtd) <- prolog
       return dtd )
 where extract (ExtMarkupDecl m) = [m]
       extract (ExtConditionalSect (IncludeSect i)) = concatMap extract i
       extract (ExtConditionalSect (IgnoreSect i)) = []

document :: XParser Document
document = do
    p <- prolog `elserror` "unrecognisable XML prolog"
    e <- element `elserror` "no toplevel document element"
    ms <- many misc
    (_,ge) <- stget
    return (Document p ge e)

comment :: XParser Comment
comment = do
    bracket (tok TokCommentOpen) freetext (tok TokCommentClose)

processinginstruction :: XParser ProcessingInstruction
processinginstruction = do
    tok TokPIOpen
    n <- string  `elserror` "processing instruction has no target"
    f <- freetext
    tok TokPIClose `elserror` "missing ?>"
    return (n, f)

cdsect :: XParser CDSect
cdsect = do
    tok TokSectionOpen
    bracket (tok (TokSection CDATAx)) chardata (tok TokSectionClose)

prolog :: XParser Prolog
prolog = do
    x <- maybe xmldecl
    many misc
    dtd <- maybe doctypedecl
    many misc
    return (Prolog x dtd)

xmldecl :: XParser XMLDecl
xmldecl = do
    tok TokPIOpen
    (word "xml" +++ word "XML")
    p <- posn
    s <- freetext
    tok TokPIClose `elserror` "missing ?> in <?xml ...?>"
    raise ((papply' aux emptySTs . xmlReLex p) s)
  where
    aux = do
      v <- versioninfo  `elserror` "missing XML version info"
      e <- maybe encodingdecl
      s <- maybe sddecl
      return (XMLDecl v e s)
    raise (Left err) = mzero `elserror` err
    raise (Right ok) = (return . fst3 . head) ok

versioninfo :: XParser VersionInfo
versioninfo = do
    (word "version" +++ word "VERSION")
    tok TokEqual
    bracket (tok TokQuote) freetext (tok TokQuote)

misc :: XParser Misc
misc =
    ( comment >>= return . Comment) +++
    ( processinginstruction >>= return . PI)

doctypedecl :: XParser DocTypeDecl
doctypedecl = do
    tok TokSpecialOpen
    tok (TokSpecial DOCTYPEx)
    n <- name
    eid <- maybe externalid
    es <- maybe (bracket (tok TokSqOpen)
                         (many (peRef markupdecl))
                         (tok TokSqClose))
    blank (tok TokAnyClose)  `elserror` "missing > in DOCTYPE decl"
    return (DTD n eid (case es of { Nothing -> []; Just e -> e }))

markupdecl :: XParser MarkupDecl
markupdecl =
    ( elementdecl >>= return . Element) +++
    ( attlistdecl >>= return . AttList) +++
    ( entitydecl >>= return . Entity) +++
    ( notationdecl >>= return . Notation) +++
    ( misc >>= return . MarkupMisc)

extsubset :: XParser ExtSubset
extsubset = do
    td <- maybe textdecl
    ds <- many (peRef extsubsetdecl)
    return (ExtSubset td ds)

extsubsetdecl :: XParser ExtSubsetDecl
extsubsetdecl =
    ( markupdecl >>= return . ExtMarkupDecl) +++
    ( conditionalsect >>= return . ExtConditionalSect)

sddecl :: XParser SDDecl
sddecl = do
    (word "standalone" +++ word "STANDALONE")
    tok TokEqual `elserror` "missing = in 'standalone' decl"
    bracket (tok TokQuote)
            ( (word "yes" >> return True) +++
              (word "no" >> return False) `elserror`
              "'standalone' decl requires 'yes' or 'no' value" )
            (tok TokQuote)

element :: XParser Element
element = do
    tok TokAnyOpen
    (ElemTag n as) <- elemtag
    (( do tok TokEndClose
          return (Elem n as [])) +++
     ( do tok TokAnyClose
          cs <- many content
          p <- posn
          m <- bracket (tok TokEndOpen) name (tok TokAnyClose)
          checkmatch p n m
          return (Elem n as cs))
     `elserror` "missing > or /> in element tag")

checkmatch :: Posn -> Name -> Name -> XParser ()
checkmatch p n m =
  if n == m then return ()
  else mzero `elserror` ("tag <"++n++"> terminated by </"++m++">")

elemtag :: XParser ElemTag
elemtag = do
    n <- name `elserror` "malformed element tag"
    as <- many attribute
    return (ElemTag n as)

attribute :: XParser Attribute
attribute = do
    n <- name
    tok TokEqual `elserror` "missing = in attribute"
    v <- attvalue `elserror` "missing attvalue"
    return (n,v)

content :: XParser Content
content =
    ( element >>= return . CElem) +++
    ( chardata >>= return . CString False) +++
    ( reference >>= return . CRef) +++
    ( cdsect >>= return . CString True) +++
    ( misc >>= return . CMisc)

elementdecl :: XParser ElementDecl
elementdecl = do
    tok TokSpecialOpen
    tok (TokSpecial ELEMENTx)
    n <- peRef name `elserror` "missing identifier in ELEMENT decl"
    c <- peRef contentspec `elserror` "missing content spec in ELEMENT decl"
    blank (tok TokAnyClose) `elserror` "expected > terminating ELEMENT decl"
    return (ElementDecl n c)

contentspec :: XParser ContentSpec
contentspec =
    ( peRef (word "EMPTY") >> return EMPTY) +++
    ( peRef (word "ANY") >> return ANY) +++
    ( peRef mixed >>= return . Mixed) +++
    ( peRef cp >>= return . ContentSpec)

choice :: XParser [CP]
choice = do
    bracket (tok TokBraOpen `debug` "Trying choice")
            (peRef cp `sepby1` blank (tok TokPipe))
            (blank (tok TokBraClose `debug` "Succeeded with choice"))

sequence :: XParser [CP]
sequence = do
    bracket (tok TokBraOpen `debug` "Trying sequence")
            (peRef cp `sepby1` blank (tok TokComma))
            (blank (tok TokBraClose `debug` "Succeeded with sequence"))

cp :: XParser CP
cp =
    ( do n <- name
         m <- modifier
         let c = TagName n m
         return c `debug` ("ContentSpec: name "++show c)) +++
    ( do ss <- sequence
         m <- modifier
         let c = Seq ss m
         return c `debug` ("ContentSpec: sequence "++show c)) +++
    ( do cs <- choice
         m <- modifier
         let c = Choice cs m
         return c `debug` ("ContentSpec: choice "++show c))

modifier :: XParser Modifier
modifier =
    ( tok TokStar >> return Star) +++
    ( tok TokQuery >> return Query) +++
    ( tok TokPlus >> return Plus) +++
    ( return None)

-- just for debugging
instance Show CP where
    show (TagName n m) = n++show m
    show (Choice cps m) = '(': concat (intersperse "|" (map show cps))
                          ++")"++show m
    show (Seq cps m) = '(': concat (intersperse "," (map show cps))
                          ++")"++show m
instance Show Modifier where
    show None = ""
    show Query = "?"
    show Star = "*"
    show Plus = "+"
----

mixed :: XParser Mixed
mixed = do
    tok TokBraOpen
    peRef (do tok TokHash
              word "PCDATA")
    cont
  where
    cont = ( do cs <- many (peRef (do tok TokPipe
                                      peRef name))
                blank (tok TokBraClose >> tok TokStar)
                return (PCDATAplus cs)) +++
           ( blank (tok TokBraClose >> tok TokStar) >> return PCDATA) +++
           ( blank (tok TokBraClose) >> return PCDATA)

attlistdecl :: XParser AttListDecl
attlistdecl = do
    tok TokSpecialOpen
    tok (TokSpecial ATTLISTx)
    n <- peRef name `elserror` "missing identifier in ATTLIST"
    ds <- peRef (many (peRef attdef))
    blank (tok TokAnyClose) `elserror` "missing > terminating ATTLIST"
    return (AttListDecl n ds)

attdef :: XParser AttDef
attdef =
  do n <- peRef name
     t <- peRef atttype `elserror` "missing attribute type in attlist defn"
     d <- peRef defaultdecl
     return (AttDef n t d)

atttype :: XParser AttType
atttype =
    ( word "CDATA" >> return StringType) +++
    ( tokenizedtype >>= return . TokenizedType) +++
    ( enumeratedtype >>= return . EnumeratedType)

tokenizedtype :: XParser TokenizedType
tokenizedtype =
    ( word "ID" >> return ID) +++
    ( word "IDREF" >> return IDREF) +++
    ( word "IDREFS" >> return IDREFS) +++
    ( word "ENTITY" >> return ENTITY) +++
    ( word "ENTITIES" >> return ENTITIES) +++
    ( word "NMTOKEN" >> return NMTOKEN) +++
    ( word "NMTOKENS" >> return NMTOKENS)

enumeratedtype :: XParser EnumeratedType
enumeratedtype =
    ( notationtype >>= return . NotationType) +++
    ( enumeration >>= return . Enumeration)

notationtype :: XParser NotationType
notationtype = do
    word "NOTATION"
    bracket (tok TokBraOpen)
            (peRef name `sepby1` peRef (tok TokPipe))
            (blank (tok TokBraClose))

enumeration :: XParser Enumeration
enumeration =
    bracket (tok TokBraOpen)
            (peRef nmtoken `sepby1` peRef ((tok TokPipe)))
            (blank (tok TokBraClose))

defaultdecl :: XParser DefaultDecl
defaultdecl =
    ( tok TokHash >> word "REQUIRED" >> return REQUIRED) +++
    ( tok TokHash >> word "IMPLIED" >> return IMPLIED) +++
    ( do f <- maybe (tok TokHash >> word "FIXED" >> return FIXED)
         a <- peRef attvalue
         return (DefaultTo a f))

conditionalsect :: XParser ConditionalSect
conditionalsect =
    ( do tok TokSectionOpen
         peRef (tok (TokSection INCLUDEx))
         tok TokSqOpen `elserror` "missing [ after INCLUDE"
         i <- many (peRef extsubsetdecl)
         tok TokSectionClose `elserror` "missing ]]> for INCLUDE section"
         return (IncludeSect i)) +++
    ( do tok TokSectionOpen
         peRef (tok (TokSection IGNOREx))
         tok TokSqOpen `elserror` "missing [ after IGNORE"
         i <- many newIgnore  -- many ignoresectcontents
         tok TokSectionClose `elserror` "missing ]]> for IGNORE section"
         return (IgnoreSect []))

newIgnore :: XParser Ignore
newIgnore =
    ( do tok TokSectionOpen
         many newIgnore `debug` "IGNORING conditional section"
         tok TokSectionClose
         return Ignore `debug` "end of IGNORED conditional section") +++
    ( do t <- nottok [TokSectionOpen,TokSectionClose]
         return Ignore  `debug` ("ignoring: "++show t))

--- obsolete?
ignoresectcontents :: XParser IgnoreSectContents
ignoresectcontents = do
    i <- ignore
    is <- many (do tok TokSectionOpen
                   ic <- ignoresectcontents
                   tok TokSectionClose
                   ig <- ignore
                   return (ic,ig))
    return (IgnoreSectContents i is)

ignore :: XParser Ignore
ignore = do
  is <- many1 (nottok [TokSectionOpen,TokSectionClose])
  return Ignore  `debug` ("ignored all of: "++show is)
----

reference :: XParser Reference
reference = do
    bracket (tok TokAmp) (freetext >>= val) (tok TokSemi)
  where
    val ('#':'x':i) | all isHexDigit i
                    = return . RefChar . fst . head . readHex $ i
    val ('#':i)     | all isDigit i
                    = return . RefChar . fst . head . readDec $ i
    val name        = return . RefEntity $ name

{- -- following is incorrect
reference =
    ( charref >>= return . RefChar) +++
    ( entityref >>= return . RefEntity)

entityref :: XParser EntityRef
entityref = do
    bracket (tok TokAmp) name (tok TokSemi)

charref :: XParser CharRef
charref = do
    bracket (tok TokAmp) (freetext >>= readCharVal) (tok TokSemi)
  where
    readCharVal ('#':'x':i) = return . fst . head . readHex $ i
    readCharVal ('#':i)     = return . fst . head . readDec $ i
    readCharVal _           = mzero
-}

pereference :: XParser PEReference
pereference = do
    bracket (tok TokPercent) nmtoken (tok TokSemi)

entitydecl :: XParser EntityDecl
entitydecl =
    ( gedecl >>= return . EntityGEDecl) +++
    ( pedecl >>= return . EntityPEDecl)

gedecl :: XParser GEDecl
gedecl = do
    tok TokSpecialOpen
    tok (TokSpecial ENTITYx)
    n <- name
    e <- entitydef `elserror` "missing entity defn in G ENTITY decl"
    tok TokAnyClose `elserror` "expected > terminating G ENTITY decl"
    stupd (addGE n e)
    return (GEDecl n e)

pedecl :: XParser PEDecl
pedecl = do
    tok TokSpecialOpen
    tok (TokSpecial ENTITYx)
    tok TokPercent
    n <- name
    e <- pedef `elserror` "missing entity defn in P ENTITY decl"
    tok TokAnyClose `elserror` "expected > terminating P ENTITY decl"
    stupd (addPE n e)
    return (PEDecl n e)

entitydef :: XParser EntityDef
entitydef =
    ( entityvalue >>= return . DefEntityValue) +++
    ( do eid <- externalid
         ndd <- maybe ndatadecl
         return (DefExternalID eid ndd))

pedef :: XParser PEDef
pedef =
    ( entityvalue >>= return . PEDefEntityValue) +++
    ( externalid >>= return . PEDefExternalID)

externalid :: XParser ExternalID
externalid =
    ( do word "SYSTEM"
         s <- systemliteral
         return (SYSTEM s)) +++
    ( do word "PUBLIC"
         p <- pubidliteral
         s <- systemliteral
         return (PUBLIC p s))

ndatadecl :: XParser NDataDecl
ndatadecl = do
    word "NDATA"
    n <- name
    return (NDATA n)

textdecl :: XParser TextDecl
textdecl = do
    tok TokPIOpen
    (word "xml" +++ word "XML")
    v <- maybe versioninfo
    e <- encodingdecl
    tok TokPIClose `elserror` "expected ?> terminating text decl"
    return (TextDecl v e)

extparsedent :: XParser ExtParsedEnt
extparsedent = do
    t <- maybe textdecl
    c <- content
    return (ExtParsedEnt t c)

extpe :: XParser ExtPE
extpe = do
    t <- maybe textdecl
    e <- many (peRef extsubsetdecl)
    return (ExtPE t e)

encodingdecl :: XParser EncodingDecl
encodingdecl = do
    (word "encoding" +++ word "ENCODING")
    tok TokEqual `elserror` "expected = in 'encoding' decl"
    f <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (EncodingDecl f)

notationdecl :: XParser NotationDecl
notationdecl = do
    tok TokSpecialOpen
    tok (TokSpecial NOTATIONx)
    n <- name
    e <- either externalid publicid
    tok TokAnyClose `elserror` "expected > terminating NOTATION decl"
    return (NOTATION n e)

publicid :: XParser PublicID
publicid = do
    word "PUBLIC"
    p <- pubidliteral
    return (PUBLICID p)

entityvalue :: XParser EntityValue
entityvalue = do
    evs <- bracket (tok TokQuote) (many (peRef ev)) (tok TokQuote)
    return (EntityValue evs)

ev :: XParser EV
ev =
    ( freetext >>= return . EVString) +++
    ( reference >>= return . EVRef)

attvalue :: XParser AttValue
attvalue = do
    avs <- bracket (tok TokQuote)
                   (many (either freetext reference))
                   (tok TokQuote)
    return (AttValue avs)

systemliteral :: XParser SystemLiteral
systemliteral = do
    s <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (SystemLiteral s)            -- note: need to fold &...; escapes

pubidliteral :: XParser PubidLiteral
pubidliteral = do
    s <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (PubidLiteral s)             -- note: need to fold &...; escapes

chardata :: XParser CharData
chardata = freetext


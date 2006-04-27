-- | This is a parser for HTML documents.  Unlike for XML documents, it
--   must include a certain amount of error-correction to account for
--   HTML features like self-terminating tags, unterminated tags, and
--   incorrect nesting.  The input is tokenised by the
--   XML lexer (a separate lexer is not required for HTML).

-- It uses a slightly extended version of the Hutton/Meijer parser
-- combinators.

module Text.XML.HaXml.Html.Parse
  ( htmlParse
  ) where

import Prelude hiding (either,maybe,sequence)
import qualified Prelude (either)
import Data.Maybe hiding (maybe)
import Data.Char (toLower, isSpace, isDigit, isHexDigit)
import Numeric (readDec,readHex)
import Control.Monad

import Text.XML.HaXml.Types
import Text.XML.HaXml.Lex
import Text.ParserCombinators.HuttonMeijerWallace

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
debug :: Monad m => String -> m ()
debug s = trace s (return ())
#else
debug :: Monad m => String -> m ()
debug s = return ()
#endif


-- | The first argument is the name of the file, the second is the string
--   contents of the file.  The result is the generic representation of
--   an XML document.
htmlParse :: String -> String -> Document
htmlParse name = simplify . sanitycheck . Prelude.either error id
                 . papply' document () . xmlLex name

sanitycheck :: Show p => [(a,s,[Either String (p,t)])] -> a
sanitycheck [] = error "***Error at line 0: document not HTML?"
sanitycheck ((x,_,[]):_) = x
sanitycheck ((x,_,s@(Right (n,_):_)):xs) =
  error ("***Error at "++show n++": data beyond end of parsed document")


---- Document simplification ----

simplify :: Document -> Document
simplify (Document p st (Elem n avs cs)) =
    Document p st (Elem n avs (deepfilter simp cs))
  where
    simp (CElem (Elem "null" [] [])) = False
    simp (CElem (Elem  n     _  [])) | n `elem` ["FONT","P","I","B","EM"
                                                ,"TT","BIG","SMALL"] = False
    simp (CString False s) | all isSpace s = False
    simp _ = True
    deepfilter p =
        filter p . map (\c-> case c of
                          (CElem (Elem n avs cs)) -> CElem (Elem n avs (deepfilter p cs))
                          _ -> c)

-- opening any of these, they close again immediately
selfclosingtags = ["IMG","HR","BR","META","COL","LINK","BASE"
                  ,"PARAM","AREA","FRAME","INPUT"]

--closing this, implicitly closes any of those which are contained in it
closeInnerTags =
  [ ("UL",      ["LI"])
  , ("OL",      ["LI"])
  , ("DL",      ["DT","DD"])
  , ("TR",      ["TH","TD"])
  , ("DIV",     ["P"])
  , ("THEAD",   ["TH","TR","TD"])
  , ("TFOOT",   ["TH","TR","TD"])
  , ("TBODY",   ["TH","TR","TD"])
  , ("TABLE",   ["TH","TR","TD","THEAD","TFOOT","TBODY"])
  , ("CAPTION", ["P"])
  , ("TH",      ["P"])
  , ("TD",      ["P"])
  , ("LI",      ["P"])
  , ("DT",      ["P"])
  , ("DD",      ["P"])
  , ("OBJECT",  ["P"])
  , ("MAP",     ["P"])
  , ("BODY",    ["P"])
  ]

--opening this, implicitly closes that
closes :: Name -> Name -> Bool
"A"  `closes` "A"   =  True
"LI" `closes` "LI"  =  True
"TH" `closes`  t    | t `elem` ["TH","TD"]      =  True
"TD" `closes`  t    | t `elem` ["TH","TD"]      =  True
"TR" `closes`  t    | t `elem` ["TH","TD","TR"] =  True
"DT" `closes`  t    | t `elem` ["DT","DD"]      =  True
"DD" `closes`  t    | t `elem` ["DT","DD"]      =  True
"FORM"  `closes` "FORM"      = True
"LABEL" `closes` "LABEL"     = True
_       `closes` "OPTION"    = True
"THEAD" `closes` t  | t `elem` ["COLGROUP"]          = True
"TFOOT" `closes` t  | t `elem` ["THEAD","COLGROUP"]  = True
"TBODY" `closes` t  | t `elem` ["TBODY","TFOOT","THEAD","COLGROUP"] = True
"COLGROUP" `closes` "COLGROUP"  = True
t `closes` "P"
    | t `elem` ["P","H1","H2","H3","H4","H5","H6"
               ,"HR","DIV","UL","DL","OL","TABLE"]  =  True
_ `closes` _ = False



---- Misc ----

fst3 (a,_,_) = a
snd3 (_,a,_) = a
thd3 (_,_,a) = a



---- Auxiliary Parsing Functions ----

type HParser a = Parser () (Posn,TokenT) String a

name :: HParser Name
name = do {(p,TokName s) <- item; return s}

string, freetext :: HParser String
string   = do {(p,TokName s) <- item; return s}
freetext = do {(p,TokFreeText s) <- item; return s}

maybe :: HParser a -> HParser (Maybe a)
maybe p =
    ( p >>= return . Just) +++
    ( return Nothing)

either :: HParser a -> HParser b -> HParser (Either a b)
either p q =
    ( p >>= return . Left) +++
    ( q >>= return . Right)

word :: String -> HParser ()
word s = P (\st inp-> case inp of {
               (Left err: _) -> Left err;
               (Right (p,TokName n):ts) -> if s==n then Right [((),st,ts)]
                                           else Right [];
               (Right (p,TokFreeText n):ts) -> if s==n then Right [((),st,ts)]
                                               else Right [];
               ts -> Right [] } )

posn :: HParser Posn
posn = P (\st inp-> case inp of {
                     (Left err: _)   -> Left err;
                     (Right (p,_):_) -> Right [(p,st,inp)];
                     [] -> Right [(Pn "unknown" 0 0 Nothing,st,inp)]; } )

nmtoken :: HParser NmToken
nmtoken = (string +++ freetext)


---- XML Parsing Functions ----

document :: HParser Document
document = do
    p     <- prolog `elserror` "unrecognisable XML prolog"
    es    <- many1 (element "HTML document")
    ms    <- many misc
    return (Document p emptyST (case map snd es of
                                  [e] -> e
                                  es  -> Elem "HTML" [] (map CElem es)))

comment :: HParser Comment
comment = do
    bracket (tok TokCommentOpen) freetext (tok TokCommentClose)

processinginstruction :: HParser ProcessingInstruction
processinginstruction = do
    tok TokPIOpen
    n <- string  `elserror` "processing instruction has no target"
    f <- freetext
    (tok TokPIClose +++ tok TokAnyClose) `elserror` "missing ?> or >"
    return (n, f)

cdsect :: HParser CDSect
cdsect = do
    tok TokSectionOpen
    bracket (tok (TokSection CDATAx)) chardata (tok TokSectionClose)

prolog :: HParser Prolog
prolog = do
    x <- maybe xmldecl
    many misc
    dtd <- maybe doctypedecl
    many misc
    return (Prolog x dtd)

xmldecl :: HParser XMLDecl
xmldecl = do
    tok TokPIOpen
    (word "xml" +++ word "XML")
    p <- posn
    s <- freetext
    tok TokPIClose `elserror` "missing ?> in <?xml ...?>"
    (raise . papply' aux () . xmlReLex p) s
  where
    aux = do
      v <- versioninfo  `elserror` "missing XML version info"
      e <- maybe encodingdecl
      s <- maybe sddecl
      return (XMLDecl v e s)
    raise (Left err) = mzero `elserror` err
    raise (Right ok) = (return . fst3 . head) ok

versioninfo :: HParser VersionInfo
versioninfo = do
    (word "version" +++ word "VERSION")
    tok TokEqual
    bracket (tok TokQuote) freetext (tok TokQuote)

misc :: HParser Misc
misc = 
    ( comment >>= return . Comment) +++
    ( processinginstruction >>= return . PI)


-- Question: for HTML, should we disallow in-line DTDs, allowing only externals?
-- Answer: I think so.

doctypedecl :: HParser DocTypeDecl
doctypedecl = do
    tok TokSpecialOpen
    tok (TokSpecial DOCTYPEx)
    n <- name
    eid <- maybe externalid
--  es <- maybe (bracket (tok TokSqOpen)
--                       (many markupdecl)
--                       (tok TokSqClose))
    tok TokAnyClose  `elserror` "missing > in DOCTYPE decl"
--  return (DTD n eid (case es of { Nothing -> []; Just e -> e }))
    return (DTD n eid [])

--markupdecl :: HParser MarkupDecl
--markupdecl =
--    ( elementdecl >>= return . Element) +++
--    ( attlistdecl >>= return . AttList) +++
--    ( entitydecl >>= return . Entity) +++
--    ( notationdecl >>= return . Notation) +++
--    ( misc >>= return . MarkupMisc) +++
--    PEREF(MarkupPE,markupdecl)
--
--extsubset :: HParser ExtSubset
--extsubset = do
--    td <- maybe textdecl
--    ds <- many extsubsetdecl
--    return (ExtSubset td ds)
--
--extsubsetdecl :: HParser ExtSubsetDecl
--extsubsetdecl =
--    ( markupdecl >>= return . ExtMarkupDecl) +++
--    ( conditionalsect >>= return . ExtConditionalSect) +++
--    PEREF(ExtPEReference,extsubsetdecl)

sddecl :: HParser SDDecl
sddecl = do
    (word "standalone" +++ word "STANDALONE")
    tok TokEqual `elserror` "missing = in 'standalone' decl"
    bracket (tok TokQuote)
            ( (word "yes" >> return True) +++
              (word "no" >> return False) `elserror`
              "'standalone' decl requires 'yes' or 'no' value" )
            (tok TokQuote)




----
-- VERY IMPORTANT NOTE: The stack returned here contains those tags which
-- have been closed implicitly and need to be reopened again at the
-- earliest opportunity.
type Stack = [(Name,[Attribute])]

element :: Name -> HParser (Stack,Element)
element ctx =
  do
    tok TokAnyOpen
    (ElemTag e avs) <- elemtag
    ( if e `closes` ctx then
         -- insert the missing close-tag, fail forward, and reparse.
         ( do debug ("/")
              unparse ([TokEndOpen, TokName ctx, TokAnyClose,
                        TokAnyOpen, TokName e] ++ reformatAttrs avs)
              return ([], Elem "null" [] []))
      else if e `elem` selfclosingtags then
         -- complete the parse straightaway.
         ( do tok TokEndClose	-- self-closing <tag /> 
              debug (e++"[+]")
              return ([], Elem e avs [])) +++
     --  ( do tok TokAnyClose	-- sequence <tag></tag>	(**not HTML?**)
     --       debug (e++"[+")
     --       n <- bracket (tok TokEndOpen) name (tok TokAnyClose)
     --       debug "]"
     --       if e == (map toLower n :: Name) 
     --         then return ([], Elem e avs [])      
     --         else return (error "no nesting in empty tag")) +++
         ( do tok TokAnyClose	-- <tag> with no close (e.g. <IMG>)
              debug (e++"[+]")
              return ([], Elem e avs []))
      else
        (( do tok TokEndClose
              debug (e++"[]")
              return ([], Elem e avs [])) +++
         ( do tok TokAnyClose `elserror` "missing > or /> in element tag"
              debug (e++"[")
              zz <- many (content e)
           -- (if null zz then return (error ("empty content in context: "++e)) else return ())
              let (ss,cs) = unzip zz
              let s       = if null ss then [] else last ss
              n <- bracket (tok TokEndOpen) name (tok TokAnyClose)
              debug "]"
              ( if e == (map toLower n :: Name) then
                  do unparse (reformatTags (closeInner e s))
                     debug "^"
                     return ([], Elem e avs cs)
                else
                  do unparse [TokEndOpen, TokName n, TokAnyClose]
                     debug "-"
                     return (((e,avs):s), Elem e avs cs))
         ) `elserror` ("failed to repair non-matching tags in context: "++ctx)))

closeInner :: Name -> [(Name,[Attribute])] -> [(Name,[Attribute])]
closeInner c ts =
    case lookup c closeInnerTags of
      (Just these) -> filter ((`notElem` these).fst) ts
      Nothing      -> ts

unparse ts = do p <- posn
                reparse (map Right (zip (repeat p) ts))

reformatAttrs avs = concatMap f0 avs
    where f0 (a, AttValue [Left s]) = [TokName a, TokEqual, TokQuote,
                                       TokFreeText s, TokQuote]
reformatTags ts = concatMap f0 ts
    where f0 (t,avs) = [TokAnyOpen, TokName t]++reformatAttrs avs++[TokAnyClose]

content :: Name -> HParser (Stack,Content)
content ctx =
    ( element ctx >>= \(s,e)-> return (s, CElem e)) +++
    ( chardata >>= \s-> return ([], CString False s)) +++
    ( reference >>= \r-> return ([], CRef r)) +++
    ( cdsect >>= \c-> return ([], CString True c)) +++
    ( misc >>= \m->  return ([], CMisc m))

----
elemtag :: HParser ElemTag
elemtag = do
    n <- name `elserror` "malformed element tag"
    as <- many attribute
    return (ElemTag (map toLower n) as)

attribute :: HParser Attribute
attribute = do
    n <- name
    v <- (do tok TokEqual
             attvalue) +++
         (return (AttValue [Left "TRUE"]))
    return (map toLower n,v)

--elementdecl :: HParser ElementDecl
--elementdecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ELEMENTx)
--    n <- name `elserror` "missing identifier in ELEMENT decl"
--    c <- contentspec `elserror` "missing content spec in ELEMENT decl"
--    tok TokAnyClose `elserror` "expected > terminating ELEMENT decl"
--    return (ElementDecl n c)
--
--contentspec :: HParser ContentSpec
--contentspec =
--    ( word "EMPTY" >> return EMPTY) +++
--    ( word "ANY" >> return ANY) +++
--    ( mixed >>= return . Mixed) +++
--    ( cp >>= return . ContentSpec) +++
--    PEREF(ContentPE,contentspec)
--
--choice :: HParser [CP]
--choice = do
--    bracket (tok TokBraOpen)
--            (cp `sepby1` (tok TokPipe))
--            (tok TokBraClose)
--
--sequence :: HParser [CP]
--sequence = do
--    bracket (tok TokBraOpen)
--            (cp `sepby1` (tok TokComma))
--            (tok TokBraClose)
--
--cp :: HParser CP
--cp =
--    ( do n <- name
--         m <- modifier
--         return (TagName n m)) +++
--    ( do ss <- sequence
--         m <- modifier
--         return (Seq ss m)) +++
--    ( do cs <- choice
--         m <- modifier
--         return (Choice cs m)) +++
--    PEREF(CPPE,cp)
--
--modifier :: HParser Modifier
--modifier =
--    ( tok TokStar >> return Star) +++
--    ( tok TokQuery >> return Query) +++
--    ( tok TokPlus >> return Plus) +++
--    ( return None)
--
--mixed :: HParser Mixed
--mixed = do
--    tok TokBraOpen
--    tok TokHash
--    word "PCDATA"
--    cont
--  where
--    cont = ( tok TokBraClose >> return PCDATA) +++
--           ( do cs <- many ( do tok TokPipe
--                                n <- name
--                                return n)
--                tok TokBraClose
--                tok TokStar
--                return (PCDATAplus cs))
--
--attlistdecl :: HParser AttListDecl
--attlistdecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ATTLISTx)
--    n <- name `elserror` "missing identifier in ATTLIST"
--    ds <- many attdef
--    tok TokAnyClose `elserror` "missing > terminating ATTLIST"
--    return (AttListDecl n ds)
--
--attdef :: HParser AttDef
--attdef = do
--    n <- name
--    t <- atttype `elserror` "missing attribute type in attlist defn"
--    d <- defaultdecl
--    return (AttDef n t d)
--
--atttype :: HParser AttType
--atttype =
--    ( word "CDATA" >> return StringType) +++
--    ( tokenizedtype >>= return . TokenizedType) +++
--    ( enumeratedtype >>= return . EnumeratedType)
--
--tokenizedtype :: HParser TokenizedType
--tokenizedtype =
--    ( word "ID" >> return ID) +++
--    ( word "IDREF" >> return IDREF) +++
--    ( word "IDREFS" >> return IDREFS) +++
--    ( word "ENTITY" >> return ENTITY) +++
--    ( word "ENTITIES" >> return ENTITIES) +++
--    ( word "NMTOKEN" >> return NMTOKEN) +++
--    ( word "NMTOKENS" >> return NMTOKENS)
--
--enumeratedtype :: HParser EnumeratedType
--enumeratedtype =
--    ( notationtype >>= return . NotationType) +++
--    ( enumeration >>= return . Enumeration)
--
--notationtype :: HParser NotationType
--notationtype = do
--    word "NOTATION"
--    bracket (tok TokBraOpen)
--            (name `sepby1` (tok TokPipe))
--            (tok TokBraClose)
--
--enumeration :: HParser Enumeration
--enumeration =
--    bracket (tok TokBraOpen)
--            (nmtoken `sepby1` (tok TokPipe))
--            (tok TokBraClose)
--
--defaultdecl :: HParser DefaultDecl
--defaultdecl =
--    ( tok TokHash >> word "REQUIRED" >> return REQUIRED) +++
--    ( tok TokHash >> word "IMPLIED" >> return IMPLIED) +++
--    ( do f <- maybe (tok TokHash >> word "FIXED" >> return FIXED)
--         a <- attvalue
--         return (DefaultTo a f))
--
--conditionalsect :: HParser ConditionalSect
--conditionalsect =
--    ( do tok TokSectionOpen
--         tok (TokSection INCLUDEx)
--         tok TokSqOpen `elserror` "missing [ after INCLUDE"
--         i <- extsubsetdecl `elserror` "missing ExtSubsetDecl in INCLUDE"
--         tok TokSectionClose `elserror` "missing ] after INCLUDE"
--         return (IncludeSect i)) +++
--    ( do tok TokSectionOpen
--         tok (TokSection IGNOREx)
--         tok TokSqOpen `elserror` "missing [ after IGNORE"
--         i <- many ignoresectcontents
--         tok TokSectionClose `elserror` "missing ] after IGNORE"
--         return (IgnoreSect i))
--
--ignoresectcontents :: HParser IgnoreSectContents
--ignoresectcontents = do
--    i <- ignore
--    is <- many (do tok TokSectionOpen
--                   ic <- ignoresectcontents
--                   tok TokSectionClose
--                   ig <- ignore
--                   return (ic,ig))
--    return (IgnoreSectContents i is)
--
--ignore :: HParser Ignore
--ignore = freetext >>= return . Ignore

reference :: HParser Reference
reference = do
    bracket (tok TokAmp) (freetext >>= val) (tok TokSemi)
  where
    val ('#':'x':i) | all isHexDigit i
                    = return . RefChar . fst . head . readHex $ i
    val ('#':i)     | all isDigit i
                    = return . RefChar . fst . head . readDec $ i
    val name        = return . RefEntity $ name

{-
reference :: HParser Reference
reference =
    ( charref >>= return . RefChar) +++
    ( entityref >>= return . RefEntity)

entityref :: HParser EntityRef
entityref = do
    n <- bracket (tok TokAmp) name (tok TokSemi)
    return n

charref :: HParser CharRef
charref = do
    bracket (tok TokAmp) (freetext >>= readCharVal) (tok TokSemi)
  where
    readCharVal ('#':'x':i) = return . fst . head . readHex $ i
    readCharVal ('#':i)     = return . fst . head . readDec $ i
    readCharVal _           = mzero
-}

--pereference :: HParser PEReference
--pereference = do
--    bracket (tok TokPercent) nmtoken (tok TokSemi)
--
--entitydecl :: HParser EntityDecl
--entitydecl =
--    ( gedecl >>= return . EntityGEDecl) +++
--    ( pedecl >>= return . EntityPEDecl)
--
--gedecl :: HParser GEDecl
--gedecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ENTITYx)
--    n <- name
--    e <- entitydef `elserror` "missing entity defn in G ENTITY decl"
--    tok TokAnyClose `elserror` "expected > terminating G ENTITY decl"
--    return (GEDecl n e)
--
--pedecl :: HParser PEDecl
--pedecl = do
--    tok TokSpecialOpen
--    tok (TokSpecial ENTITYx)
--    tok TokPercent
--    n <- name
--    e <- pedef `elserror` "missing entity defn in P ENTITY decl"
--    tok TokAnyClose `elserror` "expected > terminating P ENTITY decl"
--    return (PEDecl n e)
--
--entitydef :: HParser EntityDef
--entitydef =
--    ( entityvalue >>= return . DefEntityValue) +++
--    ( do eid <- externalid
--         ndd <- maybe ndatadecl
--         return (DefExternalID eid ndd))
--
--pedef :: HParser PEDef
--pedef =
--    ( entityvalue >>= return . PEDefEntityValue) +++
--    ( externalid >>= return . PEDefExternalID)

externalid :: HParser ExternalID
externalid =
    ( do word "SYSTEM"
         s <- systemliteral
         return (SYSTEM s)) +++
    ( do word "PUBLIC"
         p <- pubidliteral
         s <- (systemliteral +++ return (SystemLiteral ""))
         return (PUBLIC p s))

--ndatadecl :: HParser NDataDecl
--ndatadecl = do
--    word "NDATA"
--    n <- name
--    return (NDATA n)

textdecl :: HParser TextDecl
textdecl = do
    tok TokPIOpen
    (word "xml" +++ word "XML")
    v <- maybe versioninfo
    e <- encodingdecl
    tok TokPIClose `elserror` "expected ?> terminating text decl"
    return (TextDecl v e)

--extparsedent :: HParser ExtParsedEnt
--extparsedent = do
--    t <- maybe textdecl
--    (_,c) <- (content "")
--    return (ExtParsedEnt t c)
--
--extpe :: HParser ExtPE
--extpe = do
--    t <- maybe textdecl
--    e <- extsubsetdecl
--    return (ExtPE t e)

encodingdecl :: HParser EncodingDecl
encodingdecl = do
    (word "encoding" +++ word "ENCODING")
    tok TokEqual `elserror` "expected = in 'encoding' decl"
    f <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (EncodingDecl f)

--notationdecl :: HParser NotationDecl
--notationdecl = do
--    tok TokSpecialOpen
--    word "NOTATION"
--    n <- name
--    e <- either externalid publicid
--    tok TokAnyClose `elserror` "expected > terminating NOTATION decl"
--    return (NOTATION n e)

publicid :: HParser PublicID
publicid = do
    word "PUBLICID"
    p <- pubidliteral
    return (PUBLICID p)

entityvalue :: HParser EntityValue
entityvalue = do
    evs <- bracket (tok TokQuote) (many ev) (tok TokQuote)
    return (EntityValue evs)

ev :: HParser EV
ev =
    ( freetext >>= return . EVString) +++
--  PEREF(EVPERef,ev) +++
    ( reference >>= return . EVRef)

attvalue :: HParser AttValue
attvalue =
  ( do avs <- bracket (tok TokQuote)
                      (many (either freetext reference))
                      (tok TokQuote)
       return (AttValue avs) ) +++
  ( do v <- nmtoken
       return (AttValue [Left v]) )

systemliteral :: HParser SystemLiteral
systemliteral = do
    s <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (SystemLiteral s)		-- note: need to fold &...; escapes

pubidliteral :: HParser PubidLiteral
pubidliteral = do
    s <- bracket (tok TokQuote) freetext (tok TokQuote)
    return (PubidLiteral s)		-- note: need to fold &...; escapes

chardata :: HParser CharData
chardata = freetext -- >>= return . CharData


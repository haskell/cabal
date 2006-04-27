-- | You don't normally need to use this Lex module directly - it is
--   called automatically by the parser.  (This interface is only exposed
--   for debugging purposes.)
--
-- This is a hand-written lexer for tokenising the text of an XML
-- document so that it is ready for parsing.  It attaches position
-- information in (line,column) format to every token.  The main
-- entry point is 'xmlLex'.  A secondary entry point, 'xmlReLex', is
-- provided for when the parser needs to stuff a string back onto
-- the front of the text and re-tokenise it (typically when expanding
-- macros).
--
-- As one would expect, the lexer is essentially a small finite
-- state machine.

module Text.XML.HaXml.Lex
  (
  -- * Entry points to the lexer
    xmlLex         -- :: String -> String -> [Token]
  , xmlReLex       -- :: Posn   -> String -> [Token]
  , posInNewCxt    -- :: String -> Posn
  -- * Token and position types
  , Token
  , Posn(..)
  , TokenT(..)
  , Special(..)
  , Section(..)
  ) where

import Data.Char

data Where = InTag String | NotInTag
    deriving (Eq)

-- | All tokens are paired up with a source position.
--   Lexical errors are passed back through the @Either@ type.
type Token = Either String (Posn, TokenT)

-- | Source positions contain a filename, line, column, and an
--   inclusion point, which is itself another source position,
--   recursively.
data Posn = Pn String !Int !Int (Maybe Posn)
        deriving (Eq)

instance Show Posn where
      showsPrec p (Pn f l c i) = showString f .
                                 showString "  at line " . shows l .
                                 showString " col " . shows c .
                                 ( case i of
                                    Nothing -> id
                                    Just p  -> showString "\n    used by  " .
                                               shows p )

-- | The basic token type.
data TokenT =
      TokCommentOpen		-- ^   \<!--
    | TokCommentClose		-- ^   -->
    | TokPIOpen			-- ^   \<?
    | TokPIClose		-- ^   ?>
    | TokSectionOpen		-- ^   \<![
    | TokSectionClose		-- ^   ]]>
    | TokSection Section	-- ^   CDATA INCLUDE IGNORE etc
    | TokSpecialOpen		-- ^   \<!
    | TokSpecial Special	-- ^   DOCTYPE ELEMENT ATTLIST etc
    | TokEndOpen		-- ^   \<\/
    | TokEndClose		-- ^   \/>
    | TokAnyOpen		-- ^   \<
    | TokAnyClose		-- ^   >
    | TokSqOpen			-- ^   \[
    | TokSqClose		-- ^   \]
    | TokEqual			-- ^   =
    | TokQuery			-- ^   ?
    | TokStar			-- ^   \*
    | TokPlus			-- ^   +
    | TokAmp			-- ^   &
    | TokSemi			-- ^   ;
    | TokHash			-- ^   #
    | TokBraOpen		-- ^   (
    | TokBraClose		-- ^   )
    | TokPipe			-- ^   |
    | TokPercent		-- ^   %
    | TokComma			-- ^   ,
    | TokQuote			-- ^   \'\' or \"\"
    | TokName      String	-- ^   begins with letter
    | TokFreeText  String	-- ^   any character data
    | TokNull			-- ^   fake token
    deriving (Eq)

data Special =
      DOCTYPEx
    | ELEMENTx
    | ATTLISTx
    | ENTITYx
    | NOTATIONx
    deriving (Eq,Show)
data Section =
      CDATAx
    | INCLUDEx
    | IGNOREx
    deriving (Eq,Show)

instance Show TokenT where
  showsPrec p  TokCommentOpen		= showString     "<!--"
  showsPrec p  TokCommentClose		= showString     "-->"
  showsPrec p  TokPIOpen		= showString     "<?"
  showsPrec p  TokPIClose		= showString     "?>"
  showsPrec p  TokSectionOpen		= showString     "<!["
  showsPrec p  TokSectionClose		= showString     "]]>"
  showsPrec p  (TokSection s)		= showsPrec p s
  showsPrec p  TokSpecialOpen		= showString     "<!"
  showsPrec p  (TokSpecial s)		= showsPrec p s
  showsPrec p  TokEndOpen		= showString     "</"
  showsPrec p  TokEndClose		= showString     "/>"
  showsPrec p  TokAnyOpen		= showString     "<"
  showsPrec p  TokAnyClose		= showString     ">"
  showsPrec p  TokSqOpen		= showString     "["
  showsPrec p  TokSqClose		= showString     "]"
  showsPrec p  TokEqual			= showString     "="
  showsPrec p  TokQuery			= showString     "?"
  showsPrec p  TokStar			= showString     "*"
  showsPrec p  TokPlus			= showString     "+"
  showsPrec p  TokAmp			= showString     "&"
  showsPrec p  TokSemi			= showString     ";"
  showsPrec p  TokHash			= showString     "#"
  showsPrec p  TokBraOpen		= showString     "("
  showsPrec p  TokBraClose		= showString     ")"
  showsPrec p  TokPipe			= showString     "|"
  showsPrec p  TokPercent		= showString     "%"
  showsPrec p  TokComma			= showString     ","
  showsPrec p  TokQuote			= showString     "' or \""
  showsPrec p  (TokName      s)		= showString     s
  showsPrec p  (TokFreeText  s)		= showString     s
  showsPrec p  TokNull			= showString     "(null)"

--trim, revtrim :: String -> String
--trim    = f . f         where f = reverse . dropWhile isSpace
--revtrim = f.reverse.f   where f = dropWhile isSpace
--revtrim = reverse . dropWhile (=='\n')  -- most recently used defn.

emit :: TokenT -> Posn -> Token
emit tok p = forcep p `seq` Right (p,tok)

lexerror :: String -> Posn -> [Token]
lexerror s p = [Left ("Lexical error in  "++show p++":\n  "++s)]

forcep (Pn f n m i) = m `seq` n

addcol :: Int -> Posn -> Posn
addcol n (Pn f r c i) = Pn f r (c+n) i

newline, tab :: Posn -> Posn
newline (Pn f r c i) = Pn f (r+1) 1 i
tab     (Pn f r c i) = Pn f r (((c`div`8)+1)*8) i

white :: Char -> Posn -> Posn
white ' '  = addcol 1
white '\n' = newline
white '\r' = id
white '\t' = tab
white '\xa0' = addcol 1

skip :: Int -> Posn -> String -> (Posn->String->[Token]) -> [Token]
skip n p s k = k (addcol n p) (drop n s)

blank :: ([Where]->Posn->String->[Token]) -> [Where]-> Posn-> String-> [Token]
blank k  (InTag t:_) p [] = lexerror ("unexpected EOF within "++t) p
blank k          _   p [] = []
blank k      w p (' ': s) = blank k w (addcol 1 p) s
blank k      w p ('\t':s) = blank k w (tab p) s
blank k      w p ('\n':s) = blank k w (newline p) s
blank k      w p ('\r':s) = blank k w  p s
blank k   w p ('\xa0': s) = blank k w (addcol 1 p) s
blank k      w p    s     = k w p s

prefixes :: String -> String -> Bool
[]     `prefixes`   ys   = True
(x:xs) `prefixes` (y:ys) = x==y && xs `prefixes` ys
(x:xs) `prefixes`   []   = False --error "unexpected EOF in prefix"

accumulateUntil (c:cs) tok acc pos  p  [] k =
    lexerror ("unexpected EOF while looking for closing token "++c:cs
              ++"\n  to match the opening token in "++show pos) p
accumulateUntil (c:cs) tok acc pos  p (s:ss) k
    | c==s && cs `prefixes` ss  = emit (TokFreeText (reverse acc)) pos:
                                  emit tok p: skip (length cs) (addcol 1 p) ss k
    | isSpace s  = accumulateUntil (c:cs) tok (s:acc) pos (white s p) ss k
    | otherwise  = accumulateUntil (c:cs) tok (s:acc) pos (addcol 1 p) ss k

----
-- | @posInNewCxt name pos@ creates a new source position from an old one.
--   It is used when opening a new file (e.g. a DTD inclusion), to denote
--   the start of the file @name@, but retain the stacked information that
--   it was included from the old @pos@.
posInNewCxt :: String -> Maybe Posn -> Posn
posInNewCxt name pos = Pn name 1 1 pos

-- | The first argument to 'xmlLex' is the filename (used for source positions,
--   especially in error messages), and the second is the string content of
--   the XML file.
xmlLex :: String -> String -> [Token]
xmlLex filename = xmlAny [] (posInNewCxt ("file "++filename) Nothing)

-- | 'xmlReLex' is used when the parser expands a macro (PE reference).
--    The expansion of the macro must be re-lexed as if for the first time.
xmlReLex :: Posn -> String -> [Token]
xmlReLex p s
      | "INCLUDE"  `prefixes` s  = emit (TokSection INCLUDEx) p: k 7
      | "IGNORE"   `prefixes` s  = emit (TokSection IGNOREx) p:  k 6
      | otherwise = blank xmlAny [] p s
  where
    k n = skip n p s (blank xmlAny [])

--xmltop :: Posn -> String -> [Token]
--xmltop p [] = []
--xmltop p s
--    | "<?"   `prefixes` s = emit TokPIOpen p:      next 2 (xmlPI [InTag "<?...?>"])
--    | "<!--" `prefixes` s = emit TokCommentOpen p: next 4 (xmlComment [])
--    | "<!"   `prefixes` s = emit TokSpecialOpen p: next 2 (xmlSpecial [InTag "<!...>"])
--    | otherwise           = lexerror "expected <?xml?> or <!DOCTYPE>" p
--  where next n k = skip n p s k

xmlPI      w p s = xmlName p s "name of processor in <? ?>" (blank xmlPIEnd w)
xmlPIEnd   w p s = accumulateUntil "?>"  TokPIClose "" p p s
                                                      (blank xmlAny (tail w))
xmlComment w p s = accumulateUntil "-->" TokCommentClose "" p p s
                                                             (blank xmlAny w)

-- Note: the order of the clauses in xmlAny is very important.
-- Some matches must precede the NotInTag test, the rest must follow it.
xmlAny :: [Where] -> Posn -> String -> [Token]
xmlAny  (InTag t:_)  p [] = lexerror ("unexpected EOF within "++t) p
xmlAny          _    p [] = []
xmlAny w p s@('<':ss)
    | "?"   `prefixes` ss = emit TokPIOpen p:
                                         skip 2 p s (xmlPI (InTag "<?...?>":w))
    | "!--" `prefixes` ss = emit TokCommentOpen p: skip 4 p s (xmlComment w)
    | "!["  `prefixes` ss = emit TokSectionOpen p: skip 3 p s (xmlSection w)
    | "!"   `prefixes` ss = emit TokSpecialOpen p:
                                     skip 2 p s (xmlSpecial (InTag "<!...>":w))
    | "/"   `prefixes` ss = emit TokEndOpen p: 
                                    skip 2 p s (xmlTag (InTag "</...>":tail w))
    | otherwise           = emit TokAnyOpen p:
                                 skip 1 p s (xmlTag (InTag "<...>":NotInTag:w))
xmlAny (_:_:w) p s@('/':ss)
    | ">"   `prefixes` ss = emit TokEndClose p: skip 2 p s (xmlAny w)
xmlAny w p ('&':ss) = emit TokAmp p:      accumulateUntil ";" TokSemi "" p
                                                     (addcol 1 p) ss (xmlAny w)
xmlAny w@(NotInTag:_) p s = xmlContent "" w p p s
xmlAny w p ('>':ss) = emit TokAnyClose p:       xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('[':ss) = emit TokSqOpen p:
                                 blank xmlAny (InTag "[...]":w) (addcol 1 p) ss
xmlAny w p (']':ss)
    | "]>" `prefixes` ss  =
                 emit TokSectionClose p:  skip 3 p (']':ss) (xmlAny (tail w))
    | otherwise  =    emit TokSqClose p:  blank xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('(':ss) = emit TokBraOpen p:
                                 blank xmlAny (InTag "(...)":w) (addcol 1 p) ss
xmlAny w p (')':ss) = emit TokBraClose p: blank xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('=':ss) = emit TokEqual p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('*':ss) = emit TokStar p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('+':ss) = emit TokPlus p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('?':ss) = emit TokQuery p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('|':ss) = emit TokPipe p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('%':ss) = emit TokPercent p:  blank xmlAny w (addcol 1 p) ss
xmlAny w p (';':ss) = emit TokSemi p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p (',':ss) = emit TokComma p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('#':ss) = emit TokHash p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('"':ss) = emit TokQuote p:    accumulateUntil "\"" TokQuote "" p1
                                                          p1 ss (xmlAny w)
                                             where p1 = addcol 1 p
xmlAny w p ('\'':ss) = emit TokQuote p:   accumulateUntil "'" TokQuote "" p1
                                                          p1 ss (xmlAny w)
                                             where p1 = addcol 1 p
xmlAny w p s
    | isSpace (head s)     = blank xmlAny w p s
    | isAlphaNum (head s)  = xmlName p s "some kind of name" (blank xmlAny w)
    | otherwise            = lexerror ("unrecognised token: "++take 4 s) p

xmlTag w p s = xmlName p s "tagname for element in < >" (blank xmlAny w)

xmlSection = blank xmlSection0
  where
    xmlSection0 w p s
      | "CDATA["   `prefixes` s  = emit (TokSection CDATAx) p:  accum w p s 6
      | "INCLUDE"  `prefixes` s  = emit (TokSection INCLUDEx) p:    k w p s 7
      | "IGNORE"   `prefixes` s  = emit (TokSection IGNOREx) p:     k w p s 6
      | "%"        `prefixes` s  = emit TokPercent p:               k w p s 1
      | otherwise = lexerror ("expected CDATA, IGNORE, or INCLUDE") p
    accum w p s n =
      let p0 = addcol n p in
      accumulateUntil "]]>" TokSectionClose "" p0 p0 (drop n s) (blank xmlAny w)
    k w p s n =
      skip n p s (xmlAny w)

xmlSpecial w p s
    | "DOCTYPE"  `prefixes` s = emit (TokSpecial DOCTYPEx)  p: k 7
    | "ELEMENT"  `prefixes` s = emit (TokSpecial ELEMENTx)  p: k 7
    | "ATTLIST"  `prefixes` s = emit (TokSpecial ATTLISTx)  p: k 7
    | "ENTITY"   `prefixes` s = emit (TokSpecial ENTITYx)   p: k 6
    | "NOTATION" `prefixes` s = emit (TokSpecial NOTATIONx) p: k 8
    | otherwise = lexerror
                    "expected DOCTYPE, ELEMENT, ENTITY, ATTLIST, or NOTATION" p
  where k n = skip n p s (blank xmlAny w)

xmlName p (s:ss) cxt k
    | isAlphaNum s || s==':' || s=='_'  = gatherName (s:[]) p (addcol 1 p) ss k
    | otherwise   = lexerror ("expected a "++cxt++", but got char "++show s) p
  where
    gatherName acc pos p [] k =
        emit (TokName (reverse acc)) pos: k p []
    --  lexerror ("unexpected EOF in name at "++show pos) p
    gatherName acc pos p (s:ss) k
        | isAlphaNum s || s `elem` ".-_:"
                      = gatherName (s:acc) pos (addcol 1 p) ss k
        | otherwise   = emit (TokName (reverse acc)) pos: k p (s:ss)

xmlContent acc w pos p [] = if all isSpace acc then []
                            else lexerror "unexpected EOF between tags" p
xmlContent acc w pos p (s:ss)
    | elem s "<&"    = if all isSpace acc then xmlAny w p (s:ss)
                       else emit (TokFreeText (reverse acc)) pos: xmlAny w p (s:ss)
    | isSpace s      = xmlContent (s:acc) w pos (white s p) ss
    | otherwise      = xmlContent (s:acc) w pos (addcol 1 p) ss



--ident :: (String->TokenT) ->
--          Posn -> String -> [String] ->
--         (Posn->String->[String]->[Token]) -> [Token]
--ident tok p s ss k =
--    let (name,s0) = span (\c-> isAlphaNum c || c `elem` "`-_#.'/\\") s
--    in emit (tok name) p: skip (length name) p s ss k

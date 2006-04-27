-- | This is another hand-written lexer, this time for the Xtract
--   command-language.  The entry point is lexXtract.  You don't
--   normally need to use this module directly - the lexer is called
--   automatically by the parser.  (We only expose this interface
--   for debugging purposes.)
--
--   The Xtract command language is very like the XPath specification.

module Text.XML.HaXml.Xtract.Lex
  ( lexXtract
  , Posn(..)
  , TokenT(..)
  , Token
  ) where

import Data.Char
import Data.List(isPrefixOf)


type Token = Either String (Posn, TokenT)

data Posn = Pn Int		-- char index only
        deriving Eq

instance Show Posn where
      showsPrec p (Pn c) = showString "char pos " . shows c

data TokenT =
      Symbol String
    | TokString String		--     begins with letter
    | TokNum Integer		--     begins with digit
    deriving Eq

instance Show TokenT where
    showsPrec p (Symbol s) = showString s
    showsPrec p (TokString s) = showString s
    showsPrec p (TokNum n) = shows n

emit :: TokenT -> Posn -> Token
emit tok p = forcep p `seq` Right (p,tok)
  where forcep (Pn n) = n

lexerror :: String -> Posn -> [Token]
lexerror s p = [Left ("Lexical error in selection pattern at "++show p++": "
                       ++s++"\n")]

addcol :: Int -> Posn -> Posn
addcol n (Pn c) = Pn (c+n)

newline, tab :: Posn -> Posn
newline (Pn c) = Pn (c+1)
tab     (Pn c) = Pn (((c`div`8)+1)*8)

white :: Char -> Posn -> Posn
white '\t' = tab
white ' '  = addcol 1
white '\n' = addcol 1
white '\r' = addcol 1
white '\xa0' = addcol 1

blank :: (Posn->String->[Token]) -> Posn-> String-> [Token]
blank k p []       = []
blank k p (' ': s) = blank k (addcol 1 p) s
blank k p ('\t':s) = blank k (tab p) s
blank k p ('\n':s) = blank k (newline p) s
blank k p ('\r':s) = blank k p s
blank k p ('\xa0': s) = blank k (addcol 1 p) s
blank k p    s     = k p s

----
lexXtract :: String -> [Token]
lexXtract = selAny (Pn 1)

syms = "/[]()@,=*&|~$+-<>"

selAny :: Posn -> String -> [Token]
selAny p [] = []
selAny p ('/':ss)
    | '/' == head ss  = emit (Symbol "//") p:  selAny (addcol 2 p) (tail ss)
selAny p ('!':ss)
    | '=' == head ss  = emit (Symbol "!=") p:  selAny (addcol 2 p) (tail ss)
selAny p ('<':ss)
    | '=' == head ss  = emit (Symbol "<=") p:  selAny (addcol 2 p) (tail ss)
selAny p ('>':ss)
    | '=' == head ss  = emit (Symbol ">=") p:  selAny (addcol 2 p) (tail ss)
selAny p ('\'':ss)    = emit (Symbol "'") p:
                        accumulateUntil '\'' (Symbol "'") [] p (addcol 1 p) ss selAny
selAny p ('"':ss)     = emit (Symbol "\"") p:
                        accumulateUntil '"' (Symbol "\"") [] p (addcol 1 p) ss selAny
selAny p ('_':ss)     = gatherName "_" p (addcol 1 p) ss (blank selAny)
selAny p (':':ss)     = gatherName ":" p (addcol 1 p) ss (blank selAny)
selAny p ('.':ss)
    | "=."  `isPrefixOf` ss  = emit (Symbol ".=.") p:  selAny (addcol 3 p) (drop 2 ss)
    | "!=." `isPrefixOf` ss  = emit (Symbol ".!=.") p: selAny (addcol 4 p) (drop 3 ss)
    | "<."  `isPrefixOf` ss  = emit (Symbol ".<.") p:  selAny (addcol 3 p) (drop 2 ss)
    | "<=." `isPrefixOf` ss  = emit (Symbol ".<=.") p: selAny (addcol 4 p) (drop 3 ss)
    | ">."  `isPrefixOf` ss  = emit (Symbol ".>.") p:  selAny (addcol 3 p) (drop 2 ss)
    | ">=." `isPrefixOf` ss  = emit (Symbol ".>=.") p: selAny (addcol 4 p) (drop 3 ss)
    | "/"   `isPrefixOf` ss  = emit (Symbol "./") p: selAny (addcol 2 p) (drop 1 ss)
selAny p (s:ss)
    | s `elem` syms   = emit (Symbol [s]) p:     selAny (addcol 1 p) ss
    | isSpace s       = blank selAny p (s:ss)
    | isAlpha s       = gatherName [s] p (addcol 1 p) ss (blank selAny)
    | isDigit s       = gatherNum  [s] p (addcol 1 p) ss (blank selAny)
    | otherwise       = lexerror "unrecognised pattern" p

gatherName acc pos p (s:ss) k
  | isAlphaNum s || s `elem` "-_:" = gatherName (s:acc) pos (addcol 1 p) ss k
gatherName acc pos p ss k =
  emit (TokString (reverse acc)) pos: k p ss

gatherNum acc pos p (s:ss) k
  | isHexDigit s = gatherNum (s:acc) pos (addcol 1 p) ss k
gatherNum acc pos p ss k =
  emit (TokNum (read (reverse acc))) pos: k p ss

accumulateUntil c tok acc pos  p  [] k =
    lexerror ("found end of pattern while looking for "++c
              :" to match opening quote at "++show pos) p
accumulateUntil c tok acc pos  p (s:ss) k
    | c==s       = emit (TokString (reverse acc)) pos:
                                  emit tok p: k (addcol 1 p) ss
    | isSpace s  = accumulateUntil c tok (s:acc) pos (white s p) ss k
    | otherwise  = accumulateUntil c tok (s:acc) pos (addcol 1 p) ss k

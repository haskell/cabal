{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Parsec.Parser
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
module Distribution.Parsec.Parser (
  -- * Types
  Field(..),
  Name(..),
  FieldLine(..),
  SectionArg(..),
  -- * Grammar and parsing
  -- $grammar
  readFields,
  readFields'
  ) where

import Distribution.Parsec.Lexer
import Distribution.Parsec.LexerMonad (unLex, LexState(..), LexResult(..), Position(..), LexWarning)

import Text.Parsec.Prim
import Text.Parsec.Combinator hiding (eof, notFollowedBy)
import Text.Parsec.Pos
import Text.Parsec.Error

import Control.Monad (guard, liftM2)
import Data.Char as Char
import Data.Functor.Identity
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

#ifdef CABAL_PARSEC_DEBUG
import qualified Data.Text   as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
#endif

data LexState' = LexState' !LexState (LToken, LexState')

mkLexState' :: LexState -> LexState'
mkLexState' st = LexState' st
                   (case unLex lexToken st of LexResult st' tok -> (tok, mkLexState' st'))

type Parser a = ParsecT LexState' () Identity a

instance Stream LexState' Identity LToken where
  uncons (LexState' _ (tok, st')) =
    case tok of
      L _ EOF -> return Nothing
      _       -> return (Just (tok, st'))

-- | Get lexer warnings accumulated so far
getLexerWarnings :: Parser [LexWarning]
getLexerWarnings = do
  LexState' (LexState { warnings = ws }) _ <- getInput
  return ws

setLexerMode :: Int -> Parser ()
setLexerMode code = do
  LexState' ls _ <- getInput
  setInput $! mkLexState' ls { curCode = code }

getToken :: (Token -> Maybe a) -> Parser a
getToken getTok = getTokenWithPos (\(L _ t) -> getTok t)

getTokenWithPos :: (LToken -> Maybe a) -> Parser a
getTokenWithPos getTok = tokenPrim (\(L _ t) -> describeToken t) updatePos getTok
  where
    updatePos :: SourcePos -> LToken -> LexState' -> SourcePos
    updatePos pos (L (Position col line) _) _ = newPos (sourceName pos) col line

describeToken :: Token -> String
describeToken t = case t of
  TokSym   s      -> "name "   ++ show s
  TokStr   s      -> "string " ++ show s
  TokNum   s      -> "number " ++ show s
  TokOther s      -> "symbol " ++ show s
  Indent _        -> "new line"
  TokFieldLine _  -> "field content"
  Colon           -> "\":\""
  OpenBrace       -> "\"{\""
  CloseBrace      -> "\"}\""
--  SemiColon       -> "\";\""
  EOF             -> "end of file"
  LexicalError is -> "character in input " ++ show (B.head is)

tokName :: Parser (Name Position)
tokName', tokStr, tokNum, tokOther :: Parser (SectionArg Position)
tokIndent :: Parser Int
tokColon, tokOpenBrace, tokCloseBrace :: Parser ()
tokFieldLine :: Parser (FieldLine Position)

tokName       = getTokenWithPos $ \t -> case t of L pos (TokSym x) -> Just (mkName pos x);  _ -> Nothing
tokName'      = getTokenWithPos $ \t -> case t of L pos (TokSym x) -> Just (SecArgName pos x);  _ -> Nothing
tokStr        = getTokenWithPos $ \t -> case t of L pos (TokStr   x) -> Just (SecArgStr pos x);  _ -> Nothing
tokNum        = getTokenWithPos $ \t -> case t of L pos (TokNum   x) -> Just (SecArgNum pos x);  _ -> Nothing
tokOther      = getTokenWithPos $ \t -> case t of L pos (TokOther x) -> Just (SecArgOther pos x);  _ -> Nothing
tokIndent     = getToken $ \t -> case t of Indent   x -> Just x;  _ -> Nothing
tokColon      = getToken $ \t -> case t of Colon      -> Just (); _ -> Nothing
tokOpenBrace  = getToken $ \t -> case t of OpenBrace  -> Just (); _ -> Nothing
tokCloseBrace = getToken $ \t -> case t of CloseBrace -> Just (); _ -> Nothing
tokFieldLine  = getTokenWithPos $ \t -> case t of L pos (TokFieldLine s) -> Just (FieldLine pos s); _ -> Nothing

colon, openBrace, closeBrace :: Parser ()

sectionArg :: Parser (SectionArg Position)
sectionArg   = tokName' <|> tokStr
            <|> tokNum <|> tokOther <?> "section parameter"

fieldSecName :: Parser (Name Position)
fieldSecName = tokName              <?> "field or section name"

colon        = tokColon      <?> "\":\""
openBrace    = tokOpenBrace  <?> "\"{\""
closeBrace   = tokCloseBrace <?> "\"}\""

fieldContent :: Parser (FieldLine Position)
fieldContent = tokFieldLine <?> "field contents"

newtype IndentLevel = IndentLevel Int

zeroIndentLevel :: IndentLevel
zeroIndentLevel = IndentLevel 0

incIndentLevel :: IndentLevel -> IndentLevel
incIndentLevel (IndentLevel i) = IndentLevel (succ i)

indentOfAtLeast :: IndentLevel -> Parser IndentLevel
indentOfAtLeast (IndentLevel i) = try $ do
  j <- tokIndent
  guard (j >= i) <?> "indentation of at least " ++ show i
  return (IndentLevel j)


newtype LexerMode = LexerMode Int

inLexerMode :: LexerMode -> Parser p -> Parser p
inLexerMode (LexerMode mode) p =
  do setLexerMode mode; x <- p; setLexerMode in_section; return x

data Field ann = Field   !(Name ann) [FieldLine ann]
               | Section !(Name ann) [SectionArg ann] [Field ann]
               | IfElseBlock [SectionArg ann] [Field ann] [Field ann]
  deriving (Eq, Show, Functor)

data Name ann  = Name       !ann !ByteString
  deriving (Eq, Show, Functor)

mkName :: ann -> ByteString -> Name ann
mkName ann bs = Name ann (B.map Char.toLower bs)

getName :: Name a -> ByteString
getName (Name _ bs) = bs

data FieldLine ann  = FieldLine  !ann !ByteString
  deriving (Eq, Show, Functor)

data SectionArg ann = SecArgName  !ann !ByteString
                    | SecArgStr   !ann !String
                    | SecArgNum   !ann !ByteString
                    | SecArgOther !ann !ByteString
  deriving (Eq, Show, Functor)


-----------------------
-- Cabal file grammar
--

-- $grammar
--
-- @
-- SecElems      ::= SecElem* '\n'?
-- SecElem       ::= '\n' SecElemLayout | SecElemBraces
-- SecElemLayout ::= FieldLayout | FieldBraces | SectionLayout | SectionBraces
-- SecElemBraces ::= FieldInline | FieldBraces |                 SectionBraces
-- FieldLayout   ::= name ':' line? ('\n' line)*
-- FieldBraces   ::= name ':' '\n'? '{' content '}'
-- FieldInline   ::= name ':' content
-- SectionLayout ::= name arg* SecElems
-- SectionBraces ::= name arg* '\n'? '{' SecElems '}'
-- @
--
-- and the same thing but left factored...
--
-- @
-- SecElems              ::= SecElem*
-- SecElem               ::= '\n' name SecElemLayout
--                         |      name SecElemBraces
-- SecElemLayout         ::= ':'   FieldLayoutOrBraces
--                         | arg*  SectionLayoutOrBraces
-- FieldLayoutOrBraces   ::= '\n'? '{' content '}'
--                         | line? ('\n' line)*
-- SectionLayoutOrBraces ::= '\n'? '{' SecElems '\n'? '}'
--                         | SecElems
-- SecElemBraces         ::= ':' FieldInlineOrBraces
--                         | arg* '\n'? '{' SecElems '\n'? '}'
-- FieldInlineOrBraces   ::= '\n'? '{' content '}'
--                         | content
-- @
--
-- Note how we have several productions with the sequence:
--
-- > '\n'? '{'
--
-- That is, an optional newline (and indent) followed by a @{@ token.
-- In the @SectionLayoutOrBraces@ case you can see that this makes it
-- not fully left factored (because @SecElems@ can start with a @\n@).
-- Fully left factoring here would be ugly, and though we could use a
-- lookahead of two tokens to resolve the alternatives, we can't
-- conveniently use Parsec's 'try' here to get a lookahead of only two.
-- So instead we deal with this case in the lexer by making a line
-- where the first non-space is @{@ lex as just the @{@ token, without
-- the usual indent token. Then in the parser we can resolve everything
-- with just one token of lookahead and so without using 'try'.

-- Top level of a file using cabal syntax 
--
cabalStyleFile :: Parser [Field Position]
cabalStyleFile = do es <- elements zeroIndentLevel
                    eof
                    return es

-- Elements that live at the top level or inside a section, ie fields
-- and sectionscontent
--
-- elements ::= element*
elements :: IndentLevel -> Parser [Field Position]
elements ilevel = many (element ilevel)

-- An individual element, ie a field or a section. These can either use
-- layout style or braces style. For layout style then it must start on
-- a line on it's own (so that we know its indentation level).
--
-- element ::= '\n' name elementInLayoutContext
--           |      name elementInNonLayoutContext
element :: IndentLevel -> Parser (Field Position)
element ilevel =
      (do ilevel' <- indentOfAtLeast ilevel
          name    <- fieldSecName
          elementInLayoutContext (incIndentLevel ilevel') name)
  <|> (do name    <- fieldSecName
          elementInNonLayoutContext name)

-- An element (field or section) that is valid in a layout context.
-- In a layout context we can have fields and sections that themselves
-- either use layout style or that use braces style.
--
-- elementInLayoutContext ::= ':'  fieldLayoutOrBraces
--                          | arg* sectionLayoutOrBraces
elementInLayoutContext :: IndentLevel -> Name Position -> Parser (Field Position)
elementInLayoutContext ilevel name =
      (do colon; fieldLayoutOrBraces ilevel name)
  <|> (do args  <- many sectionArg
          elems <- sectionLayoutOrBraces ilevel
          return (Section name args elems))

-- An element (field or section) that is valid in a non-layout context.
-- In a non-layout context we can have only have fields and sections that
-- themselves use braces style, or inline style fields.
--
-- elementInNonLayoutContext ::= ':' FieldInlineOrBraces
--                             | arg* '\n'? '{' elements '\n'? '}'
elementInNonLayoutContext :: Name Position -> Parser (Field Position)
elementInNonLayoutContext name =
      (do colon; fieldInlineOrBraces name)
  <|> (do args <- many sectionArg
          openBrace
          elems <- elements zeroIndentLevel
          optional tokIndent
          closeBrace
          return (Section name args elems))

-- The body of a field, using either layout style or braces style.
--
-- fieldLayoutOrBraces   ::= '\n'? '{' content '}'
--                         | line? ('\n' line)*
fieldLayoutOrBraces :: IndentLevel -> Name Position -> Parser (Field Position)
fieldLayoutOrBraces ilevel name =
      (do openBrace
          ls <- inLexerMode (LexerMode in_field_braces) (many fieldContent)
          closeBrace
          return (Field name ls))
  <|> (inLexerMode (LexerMode in_field_layout)
        (do l  <- option (FieldLine (Position 0 0) B.empty) fieldContent
                  --FIXME ^^ having to add an extra empty here is silly!
            ls <- many (do _ <- indentOfAtLeast ilevel; fieldContent)
            return (Field name (l:ls))))

-- The body of a section, using either layout style or braces style.
--
-- sectionLayoutOrBraces ::= '\n'? '{' elements \n? '}'
--                         | elements
sectionLayoutOrBraces :: IndentLevel -> Parser [Field Position]
sectionLayoutOrBraces ilevel =
      (do openBrace
          elems <- elements zeroIndentLevel
          optional tokIndent
          closeBrace
          return elems)
  <|> (elements ilevel)

-- The body of a field, using either inline style or braces.
--
-- fieldInlineOrBraces   ::= '\n'? '{' content '}'
--                         | content
fieldInlineOrBraces :: Name Position -> Parser (Field Position)
fieldInlineOrBraces name =
      (do openBrace
          ls <- inLexerMode (LexerMode in_field_braces) (many fieldContent)
          closeBrace
          return (Field name ls))
  <|> (do ls <- inLexerMode (LexerMode in_field_braces) (option [] (fmap (\l -> [l]) fieldContent))
          return (Field name ls))


readFields :: B.ByteString -> Either ParseError [Field Position]
readFields s = fmap elaborate $ parse cabalStyleFile "the input" lexSt
  where
    lexSt = mkLexState' (mkLexState s)

readFields' :: B.ByteString -> Either ParseError ([Field Position], [LexWarning])
readFields' s = parse (liftM2 (,) cabalStyleFile getLexerWarnings) "the input" lexSt
  where
    lexSt = mkLexState' (mkLexState s)

#ifdef CABAL_PARSEC_DEBUG
parseTest' p fname s =
    case parse p fname (lexSt s) of
      Left err -> putStrLn (formatError s err)

      Right x  -> print x
  where
    lexSt s = mkLexState' (mkLexState s)

parseFile :: Show a => Parser a -> FilePath -> IO ()
parseFile p f = B.readFile f >>= \s -> parseTest' p f s

parseStr  :: Show a => Parser a -> String -> IO ()
parseStr p s = parseTest' p "<input string>" (B.pack s)


formatError :: B.ByteString -> ParseError -> String
formatError input perr =
    unlines
      [ "Parse error "++ show (errorPos perr) ++ ":"
      , errLine
      , indicator ++ errmsg ]
  where
    pos       = errorPos perr
    ls        = lines' (T.decodeUtf8With T.lenientDecode input)
    errLine   = T.unpack (ls !! (sourceLine pos - 1))
    indicator = replicate (sourceColumn pos) ' ' ++ "^"
    errmsg    = showErrorMessages "or" "unknown parse error"
                                  "expecting" "unexpected" "end of file"
                                  (errorMessages perr)

lines' :: T.Text -> [T.Text]
lines' s1
  | T.null s1 = []
  | otherwise = case T.break (\c -> c == '\r' || c == '\n') s1 of
                  (l, s2) | Just (c,s3) <- T.uncons s2
                         -> case T.uncons s3 of
                              Just ('\n', s4) | c == '\r' -> l : lines' s4
                              _                           -> l : lines' s3
                          | otherwise -> [l]
#endif

eof :: Parser ()
eof = notFollowedBy anyToken <?> "end of file"
  where
    notFollowedBy :: Parser LToken -> Parser ()
    notFollowedBy p = try (    (do L _ t <- try p; unexpected (describeToken t))
                           <|> return ())
--showErrorMessages "or" "unknown parse error"
--                            "expecting" "unexpected" "end of input"

-- | Elaborate a 'Section's with @if@ name into the 'IfElseBlock's.
elaborate :: Show a => [Field a] -> [Field a]
elaborate [] = []
elaborate (field@Field{} : rest) = field : elaborate rest
elaborate (IfElseBlock args t e : rest) =
  IfElseBlock args (elaborate t) (elaborate e) : elaborate rest
elaborate (Section name args fields : Section ename [] efields : rest)
  | getName name == "if" && getName ename == "else" =
    IfElseBlock args (elaborate fields) (elaborate efields) : elaborate rest
elaborate (Section name args fields : rest)
  | getName name == "if" =
    IfElseBlock args (elaborate fields) [] : elaborate rest
  | otherwise            =
    Section name args (elaborate fields) : elaborate rest

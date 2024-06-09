{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Fields.Parser
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
{- FOURMOLU_DISABLE -}
module Distribution.Fields.Parser
  ( -- * Types
    Field (..)
  , Name (..)
  , FieldLine (..)
  , SectionArg (..)

    -- * Grammar and parsing
    -- $grammar
  , readFields
  , readFields'
#ifdef CABAL_PARSEC_DEBUG

    -- * Internal
  , parseFile
  , parseStr
  , parseBS
#endif
  ) where
{- FOURMOLU_ENABLE -}

import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Distribution.Compat.Prelude
import Distribution.Fields.Field
import Distribution.Fields.Lexer
import Distribution.Fields.LexerMonad
  ( LexResult (..)
  , LexState (..)
  , LexWarning (..)
  , LexWarningType (..)
  , unLex
  )
import Distribution.Parsec.Position (Position (..), positionCol)
import Text.Parsec.Combinator hiding (eof, notFollowedBy)
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (many, (<|>))
import Prelude ()

#ifdef CABAL_PARSEC_DEBUG
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
#endif

-- $setup
-- >>> import Data.Either (isLeft)

-- | The 'LexState'' (with a prime) is an instance of parsec's 'Stream'
-- wrapped around lexer's 'LexState' (without a prime)
data LexState' = LexState' !LexState (LToken, LexState')

mkLexState' :: LexState -> LexState'
mkLexState' st =
  LexState'
    st
    (case unLex lexToken st of LexResult st' tok -> (tok, mkLexState' st'))

type Parser a = ParsecT LexState' () Identity a

instance Stream LexState' Identity LToken where
  uncons (LexState' _ (tok, st')) =
    case tok of
      L _ EOF -> return Nothing
      -- FIXME: DEBUG: uncomment these lines to skip new tokens and restore old lexer behaviour
      -- L _ (Whitespace _) -> uncons st'
      -- L _ (Comment _) -> uncons st'
      -- FIXME: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      _ -> return (Just (tok, st'))

-- | Get lexer warnings accumulated so far
getLexerWarnings :: Parser [LexWarning]
getLexerWarnings = do
  LexState' (LexState{warnings = ws}) _ <- getInput
  return ws

addLexerWarning :: LexWarning -> Parser ()
addLexerWarning w = do
  LexState' ls@LexState{warnings = ws} _ <- getInput
  setInput $! mkLexState' ls{warnings = w : ws}

-- | Set Alex code i.e. the mode "state" lexer is in.
setLexerMode :: Int -> Parser ()
setLexerMode code = do
  LexState' ls _ <- getInput
  setInput $! mkLexState' ls{curCode = code}

getToken :: (Token -> Maybe a) -> Parser a
getToken getTok = getTokenWithPos (\(L _ t) -> getTok t)

getTokenWithPos :: (LToken -> Maybe a) -> Parser a
getTokenWithPos = tokenPrim (\(L _ t) -> describeToken t) updatePos
  where
    updatePos :: SourcePos -> LToken -> LexState' -> SourcePos
    updatePos pos (L (Position col line) _) _ = newPos (sourceName pos) col line

describeToken :: Token -> String
describeToken t = case t of
  TokSym s -> "symbol " ++ show s
  TokStr s -> "string " ++ show s
  TokOther s -> "operator " ++ show s
  Indent _ -> "new line"
  TokFieldLine _ -> "field content"
  Colon -> "\":\""
  OpenBrace -> "\"{\""
  CloseBrace -> "\"}\""
  Whitespace s -> "whitespace " ++ show s
  Comment s -> "comment " ++ show s
  EOF -> "end of file"
  LexicalError is -> "character in input " ++ show (B8.head is)

tokSym :: Parser (Name Position)
tokSym = getTokenWithPos (\t -> case t of L pos (TokSym x) -> Just (mkName pos x); _ -> Nothing)

tokSym' :: Parser (SectionArg Position)
tokSym' = getTokenWithPos (\t -> case t of L pos (TokSym x) -> Just (SecArgName pos x); _ -> Nothing)

tokStr :: Parser (SectionArg Position)
tokStr = getTokenWithPos (\t -> case t of L pos (TokStr x) -> Just (SecArgStr pos x); _ -> Nothing)

tokOther :: Parser (SectionArg Position)
tokOther = getTokenWithPos (\t -> case t of L pos (TokOther x) -> Just (SecArgOther pos x); _ -> Nothing)

tokIndent :: Parser Int
tokIndent = getToken (\t -> case t of Indent x -> Just x; _ -> Nothing)

tokColon :: Parser ()
tokColon = getToken (\t -> case t of Colon -> Just (); _ -> Nothing)

tokOpenBrace :: Parser ()
tokOpenBrace = getToken (\t -> case t of OpenBrace -> Just (); _ -> Nothing)

tokCloseBrace :: Parser ()
tokCloseBrace = getToken (\t -> case t of CloseBrace -> Just (); _ -> Nothing)

tokFieldLine :: Parser (FieldLine Position)
tokFieldLine = getTokenWithPos (\t -> case t of L pos (TokFieldLine s) -> Just (FieldLine pos s); _ -> Nothing)

tokComment :: Parser B8.ByteString
tokComment = getToken (\case Comment s -> Just s; _ -> Nothing) *> tokWhitespace

tokWhitespace :: Parser B8.ByteString
tokWhitespace = getToken (\case Whitespace s -> Just s; _ -> Nothing)

sectionArg :: Parser (SectionArg Position)
sectionArg = tokSym' <|> tokStr <|> tokOther <?> "section parameter"

fieldSecName :: Parser (Name Position)
fieldSecName = tokSym <?> "field or section name"

colon :: Parser ()
colon = tokColon <?> "\":\""

openBrace :: Parser ()
openBrace = tokOpenBrace <?> "\"{\""

closeBrace :: Parser ()
closeBrace = tokCloseBrace <?> "\"}\""

fieldContent :: Parser (FieldLine Position)
fieldContent = tokFieldLine <?> "field contents"

newtype IndentLevel = IndentLevel Int
                    deriving newtype Show

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

-----------------------
-- Cabal file grammar
--

-- $grammar
--
-- @
-- CabalStyleFile ::= SecElems
--
-- SecElems       ::= SecElem* '\\n'?
-- SecElem        ::= '\\n' SecElemLayout | SecElemBraces
-- SecElemLayout  ::= FieldLayout | FieldBraces | SectionLayout | SectionBraces
-- SecElemBraces  ::= FieldInline | FieldBraces |                 SectionBraces
-- FieldLayout    ::= name ':' line? ('\\n' line)*
-- FieldBraces    ::= name ':' '\\n'? '{' content '}'
-- FieldInline    ::= name ':' content
-- SectionLayout  ::= name arg* SecElems
-- SectionBraces  ::= name arg* '\\n'? '{' SecElems '}'
-- @
--
-- and the same thing but left factored...
--
-- @
-- SecElems              ::= SecElem*
-- SecElem               ::= '\\n' name SecElemLayout
--                         |      name SecElemBraces
-- SecElemLayout         ::= ':'   FieldLayoutOrBraces
--                         | arg*  SectionLayoutOrBraces
-- FieldLayoutOrBraces   ::= '\\n'? '{' content '}'
--                         | line? ('\\n' line)*
-- SectionLayoutOrBraces ::= '\\n'? '{' SecElems '\\n'? '}'
--                         | SecElems
-- SecElemBraces         ::= ':' FieldInlineOrBraces
--                         | arg* '\\n'? '{' SecElems '\\n'? '}'
-- FieldInlineOrBraces   ::= '\\n'? '{' content '}'
--                         | content
-- @
--
-- Note how we have several productions with the sequence:
--
-- > '\\n'? '{'
--
-- That is, an optional newline (and indent) followed by a @{@ token.
-- In the @SectionLayoutOrBraces@ case you can see that this makes it
-- not fully left factored (because @SecElems@ can start with a @\\n@).
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
cabalStyleFile = do
  skipMany tokComment
  es <- elements zeroIndentLevel
  eof
  return es

-- Elements that live at the top level or inside a section, i.e. fields
-- and sections content
--
-- elements ::= element*
elements :: IndentLevel -> Parser [Field Position]
elements ilevel = many (element ilevel)

-- An individual element, ie a field or a section. These can either use
-- layout style or braces style. For layout style then it must start on
-- a line on its own (so that we know its indentation level).
--
-- element ::= '\\n' name elementInLayoutContext
--           |      name elementInNonLayoutContext
element :: IndentLevel -> Parser (Field Position)
element ilevel = do
  result <- choice [( do
      ilevel' <- indentOfAtLeast ilevel
      name <- fieldSecName
      elementInLayoutContext (incIndentLevel ilevel') name
    ), ( do
            name <- fieldSecName
            elementInNonLayoutContext name
        )]
  result <$ many tokWhitespace

-- An element (field or section) that is valid in a layout context.
-- In a layout context we can have fields and sections that themselves
-- either use layout style or that use braces style.
--
-- elementInLayoutContext ::= ':'  fieldLayoutOrBraces
--                          | arg* sectionLayoutOrBraces
elementInLayoutContext :: IndentLevel -> Name Position -> Parser (Field Position)
elementInLayoutContext ilevel name = trace "layoutcontext" $ do
  skipMany tokWhitespace
  (do colon
      fieldLayoutOrBraces ilevel name)
    <|> ( do
            args <- many (sectionArg <* tokWhitespace)
            skipMany tokComment
            elems <- sectionLayoutOrBraces ilevel
            return (Section name args elems)
        )

-- An element (field or section) that is valid in a non-layout context.
-- In a non-layout context we can have only have fields and sections that
-- themselves use braces style, or inline style fields.
--
-- elementInNonLayoutContext ::= ':' FieldInlineOrBraces
--                             | arg* '\\n'? '{' elements '\\n'? '}'
elementInNonLayoutContext :: Name Position -> Parser (Field Position)
elementInNonLayoutContext name = trace "non-layoutcontext" $ do
  skipMany tokWhitespace
  (do colon; fieldInlineOrBraces name)
    <|> ( do
            args <- many sectionArg
            openBrace
            elems <- elements zeroIndentLevel
            optional tokIndent
            closeBrace
            return (Section name args elems)
        )

-- The body of a field, using either layout style or braces style.
--
-- fieldLayoutOrBraces   ::= '\\n'? '{' content '}'
--                         | line? ('\\n' line)*
fieldLayoutOrBraces :: IndentLevel -> Name Position -> Parser (Field Position)
fieldLayoutOrBraces ilevel name = do
  skipMany tokWhitespace
  braces <|> fieldLayout
  where
    braces = do
      openBrace
      ls <- inLexerMode (LexerMode in_field_braces) (many fieldContent)
      closeBrace
      return (Field name ls)
    fieldLayout = inLexerMode (LexerMode in_field_layout) $ do
      l <- optionMaybe fieldContent
      ls <- many (do _ <- indentOfAtLeast ilevel; fieldContent)
      return $ case l of
        Nothing -> Field name ls
        Just l' -> Field name (l' : ls)

-- The body of a section, using either layout style or braces style.
--
-- sectionLayoutOrBraces ::= '\\n'? '{' elements \\n? '}'
--                         | elements
sectionLayoutOrBraces :: IndentLevel -> Parser [Field Position]
sectionLayoutOrBraces ilevel = do
  skipMany tokWhitespace
  ( do
      openBrace
      elems <- elements zeroIndentLevel
      optional tokIndent
      closeBrace
      return elems
    )
    <|> (elements ilevel) -- TODO this used to be ilevel ??

-- The body of a field, using either inline style or braces.
--
-- fieldInlineOrBraces   ::= '\\n'? '{' content '}'
--                         | content
fieldInlineOrBraces :: Name Position -> Parser (Field Position)
fieldInlineOrBraces name = do
  skipMany tokWhitespace
  ( do
      openBrace
      ls <- inLexerMode (LexerMode in_field_braces) (many fieldContent)
      closeBrace
      return (Field name ls)
    )
    <|> ( do
            ls <- inLexerMode (LexerMode in_field_braces) (option [] (fmap (\l -> [l]) fieldContent))
            return (Field name ls)
        )

-- | Parse cabal style 'B8.ByteString' into list of 'Field's, i.e. the cabal AST.
--
-- 'readFields' assumes that input 'B8.ByteString' is valid UTF8, specifically it doesn't validate that file is valid UTF8.
-- Therefore bytestrings inside returned 'Field' will be invalid as UTF8 if the input were.
--
-- >>> readFields "foo: \223"
-- Right [Field (Name (Position 1 1) "foo") [FieldLine (Position 1 6) "\223"]]
--
-- 'readFields' won't (necessarily) fail on invalid UTF8 data, but the reported positions may be off.
--
-- __You may get weird errors on non-UTF8 input__, for example 'readFields' will fail on latin1 encoded non-breaking space:
--
-- >>> isLeft (readFields "\xa0 foo: bar")
-- True
--
-- That is rejected because parser thinks @\\xa0@ is a section name,
-- and section arguments may not contain colon.
-- If there are just latin1 non-breaking spaces, they become part of the name:
--
-- >>> readFields "\xa0\&foo: bar"
-- Right [Field (Name (Position 1 1) "\160foo") [FieldLine (Position 1 7) "bar"]]
--
-- The UTF8 non-breaking space is accepted as an indentation character (but warned about by 'readFields'').
--
-- >>> readFields' "\xc2\xa0 foo: bar"
-- Right ([Field (Name (Position 1 3) "foo") [FieldLine (Position 1 8) "bar"]],[LexWarning LexWarningNBSP (Position 1 1)])
readFields :: B8.ByteString -> Either ParseError [Field Position]
readFields s = fmap fst (readFields' s)

-- | Like 'readFields' but also return lexer warnings.
readFields' :: B8.ByteString -> Either ParseError ([Field Position], [LexWarning])
readFields' s = do
  parse parser "the input" lexSt
  where
    parser = do
      fields <- cabalStyleFile
      ws <- getLexerWarnings -- lexer accumulates warnings in reverse (consing them to the list)
      pure (fields, reverse ws ++ checkIndentation fields [])

    lexSt = mkLexState' (mkLexState s)

-- | Check (recursively) that all fields inside a block are indented the same.
--
-- We have to do this as a post-processing check.
-- As the parser uses indentOfAtLeast approach, we don't know what is the "correct"
-- indentation for following fields.
--
-- To catch during parsing we would need to parse first field/section of a section
-- and then parse the following ones (softly) requiring the exactly the same indentation.
checkIndentation :: [Field Position] -> [LexWarning] -> [LexWarning]
checkIndentation [] = id
checkIndentation (Field name _ : fs') = checkIndentation' (nameAnn name) fs'
checkIndentation (Section name _ fs : fs') = checkIndentation fs . checkIndentation' (nameAnn name) fs'

-- | We compare adjacent fields to reduce the amount of reported indentation warnings.
checkIndentation' :: Position -> [Field Position] -> [LexWarning] -> [LexWarning]
checkIndentation' _ [] = id
checkIndentation' pos (Field name _ : fs') = checkIndentation'' pos (nameAnn name) . checkIndentation' (nameAnn name) fs'
checkIndentation' pos (Section name _ fs : fs') = checkIndentation'' pos (nameAnn name) . checkIndentation fs . checkIndentation' (nameAnn name) fs'

-- | Check that positions' columns are the same.
checkIndentation'' :: Position -> Position -> [LexWarning] -> [LexWarning]
checkIndentation'' a b
  | positionCol a == positionCol b = id
  | otherwise = (LexWarning LexInconsistentIndentation b :)

#ifdef CABAL_PARSEC_DEBUG
parseTest' :: Show a => Parsec LexState' () a -> SourceName -> B8.ByteString -> IO ()
parseTest' p fname s =
  case parse p fname (lexSt s) of
    Left err -> putStrLn (formatError s err)
    Right x -> print x
  where
    lexSt = mkLexState' . mkLexState

parseFile :: Show a => Parser a -> FilePath -> IO ()
parseFile p f = B8.readFile f >>= \s -> parseTest' p f s

parseStr :: Show a => Parser a -> String -> IO ()
parseStr p = parseBS p . B8.pack

parseBS :: Show a => Parser a -> B8.ByteString -> IO ()
parseBS p = parseTest' p "<input string>"

formatError :: B8.ByteString -> ParseError -> String
formatError input perr =
  unlines
    [ "Parse error " ++ show (errorPos perr) ++ ":"
    , errLine
    , indicator ++ errmsg
    ]
  where
    pos = errorPos perr
    ls = lines' (T.decodeUtf8With T.lenientDecode input)
    errLine = T.unpack (ls !! (sourceLine pos - 1))
    indicator = replicate (sourceColumn pos) ' ' ++ "^"
    errmsg =
      showErrorMessages
        "or"
        "unknown parse error"
        "expecting"
        "unexpected"
        "end of file"
        (errorMessages perr)

-- | Handles windows/osx/unix line breaks uniformly
lines' :: T.Text -> [T.Text]
lines' s1
  | T.null s1 = []
  | otherwise = case T.break (\c -> c == '\r' || c == '\n') s1 of
      (l, s2)
        | Just (c, s3) <- T.uncons s2 ->
            case T.uncons s3 of
              Just ('\n', s4) | c == '\r' -> l : lines' s4
              _ -> l : lines' s3
        | otherwise -> [l]

#endif

eof :: Parser ()
eof = notFollowedBy anyToken <?> "end of file"
  where
    notFollowedBy :: Parser LToken -> Parser ()
    notFollowedBy p =
      try
        ( (do L _ t <- try p; unexpected (describeToken t))
            <|> return ()
        )

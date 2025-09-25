{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , readFieldsWithComments
  , readFieldsWithComments'
#ifdef CABAL_PARSEC_DEBUG

    -- * Internal
  , parseFile
  , parseStr
  , parseBS
#endif
  , formatError
  ) where
{- FOURMOLU_ENABLE -}

import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity
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

import qualified Data.Bifunctor as Bi
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T

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
      _ -> return (Just (tok, st'))

-- | A strict either for parser performance
data Either' a b = Left' !a | Right' !b

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
getTokenWithPos getTok = tokenPrim (\(L _ t) -> describeToken t) updatePos getTok
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
  TokComment c -> "comment \"" ++ B8.unpack c ++ "\""
  --  SemiColon       -> "\";\""
  EOF -> "end of file"
  LexicalError is -> "character in input " ++ show (B8.head is)

tokSym :: Parser (Name Position)
tokSym', tokStr, tokOther :: Parser (SectionArg Position)
tokIndent :: Parser Int
tokColon, tokCloseBrace :: Parser ()
tokOpenBrace :: Parser Position
tokFieldLine :: Parser (FieldLine Position)
tokSym = getTokenWithPos $ \t -> case t of L pos (TokSym x) -> Just (mkName pos x); _ -> Nothing
tokSym' = getTokenWithPos $ \t -> case t of L pos (TokSym x) -> Just (SecArgName pos x); _ -> Nothing
tokStr = getTokenWithPos $ \t -> case t of L pos (TokStr x) -> Just (SecArgStr pos x); _ -> Nothing
tokOther = getTokenWithPos $ \t -> case t of L pos (TokOther x) -> Just (SecArgOther pos x); _ -> Nothing
tokIndent = getToken $ \t -> case t of Indent x -> Just x; _ -> Nothing
tokColon = getToken $ \t -> case t of Colon -> Just (); _ -> Nothing
tokOpenBrace = getTokenWithPos $ \t -> case t of L pos OpenBrace -> Just pos; _ -> Nothing
tokCloseBrace = getToken $ \t -> case t of CloseBrace -> Just (); _ -> Nothing
tokFieldLine = getTokenWithPos $ \t -> case t of L pos (TokFieldLine s) -> Just (FieldLine pos s); _ -> Nothing

tokComment :: Parser (Comment Position)
tokComment = getTokenWithPos $ \t -> case t of L pos (TokComment c) -> Just (Comment c pos); _ -> Nothing

colon, openBrace, closeBrace :: Parser ()
sectionArg :: Parser (SectionArg Position)
sectionArg = tokSym' <|> tokStr <|> tokOther <?> "section parameter"

fieldSecName :: Parser (Name Position)
fieldSecName = tokSym <?> "field or section name"

colon = tokColon <?> "\":\""
openBrace = do
  pos <- tokOpenBrace <?> "\"{\""
  addLexerWarning (LexWarning LexBraces pos)
closeBrace = tokCloseBrace <?> "\"}\""

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

-- | This would change the state of the lexer and make interpretations of tokens different!
-- Certain lexer states are unreachable without it.
inLexerMode :: LexerMode -> Parser p -> Parser p
inLexerMode (LexerMode mode) p =
  do setLexerMode mode; x <- p; setLexerMode in_section; return x

-----------------------
-- Cabal file grammar
--
-- The non-terminals of the following grammar (symbols starting in uppercase)
-- have their corresponding parser of the same name, starting with lowercase
-- letter.

-- $grammar
--
-- @
-- CabalStyleFile             ::= Elements
--
-- Elements                   ::= Elements* '\\n'?
-- Element                    ::= '\\n' ElementInLayoutContext
--                              | ElementInNonLayoutContext
-- ElementInLayoutContext     ::= FieldLayout | FieldBraces | SectionLayout | SectionBraces
-- ElementInNonLayoutContext  ::= FieldInline | FieldBraces |                 SectionBraces
-- FieldLayout                ::= name ':' line? ('\\n' line)*
-- FieldBraces                ::= name ':' '\\n'? '{' content* '}'
-- FieldInline                ::= name ':' content
-- SectionLayout              ::= name arg* Elements
-- SectionBraces              ::= name arg* '\\n'? '{' Elements '}'
-- @
--
-- and the same thing but left factored...
--
-- @
-- Elements                   ::= Comments* (Element Comment*)*
-- Element                    ::= '\\n' name ElementInLayoutContext
--                              |      name ElementInNonLayoutContext
-- ElementInLayoutContext     ::= ':'   FieldLayoutOrBraces
--                              | arg*  SectionLayoutOrBraces
-- FieldLayoutOrBraces        ::= '\\n'? '{' comment* (content comment*)* '}'
--                              | comment* line? comment* ('\\n' line comment*)*
-- SectionLayoutOrBraces      ::= '\\n'? '{' Elements '\\n'? '}'
--                              | Elements
-- ElementInNonLayoutContext  ::= ':' FieldInlineOrBraces
--                              | arg* '\\n'? '{' Elements '\\n'? '}'
-- FieldInlineOrBraces        ::= '\\n'? '{' content '}'
--                              | content
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
cabalStyleFile :: Parser [Field (WithComments Position)]
cabalStyleFile = do
  es <- elements zeroIndentLevel
  eof
  case es of
    Left' _ -> pure [] -- We discard the comments here, because it is not a valid cabal file
    Right' es' -> pure es'

-- | Collect in annotation one or more comments after a parser succeeds
-- Careful with the 'Functor' instance!
-- If you use this with Field you might attach the same comments everywhere
commentsAfter :: Functor f => Parser (f Position) -> Parser (f (WithComments Position))
commentsAfter p = do
  x <- p
  postCmts <- many tokComment
  pure $ fmap (WithComments postCmts) x

noComments :: Functor f => f ann -> f (WithComments ann)
noComments = fmap (WithComments mempty)

-- | Returns 'Nothing' when there is no field to attach the comments to.
prependCommentsFields :: [Comment ann] -> [Field (WithComments ann)] -> Maybe [Field (WithComments ann)]
prependCommentsFields cs fs = case fs of
  [] -> Nothing
  (f : fs') -> Just $ prependCommentsField cs f : fs'

-- | We attach the comments to the name (foremost child) of 'Field', this hence cannot fail.
prependCommentsField :: [Comment ann] -> Field (WithComments ann) -> Field (WithComments ann)
prependCommentsField cs f = case f of
  (Field name fls) -> Field (mapComments (cs ++) <$> name) fls
  (Section name args fs) -> Section (mapComments (cs ++) <$> name) args fs

-- | Returns 'Nothing' when there is no field to attach the comments to.
appendCommentsFields :: [Comment ann] -> [Field (WithComments ann)] -> Maybe [Field (WithComments ann)]
appendCommentsFields cs fs = case fs of
  [] -> Nothing
  [f] -> Just [appendCommentsField cs f]
  (f : fs') -> (f :) <$> appendCommentsFields cs fs'

appendCommentsField :: [Comment ann] -> Field (WithComments ann) -> Field (WithComments ann)
appendCommentsField cs f = case f of
  (Field name fls) -> case appendCommentsFieldLines cs fls of
    Nothing -> Field (mapComments (++ cs) <$> name) []
    Just fls' -> Field name fls'
  (Section name args fs) -> case appendCommentsFields cs fs of
    Nothing -> Section (mapComments (++ cs) <$> name) args []
    Just fs' -> Section name args fs'

-- | Returns 'Nothing' when there is no field to attach the comments to.
appendCommentsFieldLines :: [Comment ann] -> [FieldLine (WithComments ann)] -> Maybe [FieldLine (WithComments ann)]
appendCommentsFieldLines cs fls = case fls of
  [] -> Nothing
  [fl] -> Just [mapComments (++ cs) <$> fl]
  (f : fls') -> (f :) <$> appendCommentsFieldLines cs fls'

-- Elements that live at the top level or inside a section, i.e. fields
-- and sections content.
--
-- This returns either many fields with their comments attached, or just the
-- comments if there are no fields to attach them to. Only at the top level it
-- is deemed correct to discard these comments, because in that case having no
-- elements isn't a valid cabal file.
--
-- elements ::= comment* (element comment*)*
elements :: IndentLevel -> Parser (Either' [Comment Position] [Field (WithComments Position)])
elements ilevel = do
  preCmts <- many tokComment
  es <- many $ do
    e <- element ilevel
    postCmts <- many tokComment
    pure $ appendCommentsField postCmts e

  case prependCommentsFields preCmts es of
    Nothing -> pure $ Left' preCmts
    Just es' -> pure $ Right' es'

-- An individual element, ie a field or a section. These can either use
-- layout style or braces style. For layout style then it must start on
-- a line on its own (so that we know its indentation level).
--
-- element ::= '\\n' name elementInLayoutContext
--           |      name elementInNonLayoutContext
element :: IndentLevel -> Parser (Field (WithComments Position))
element ilevel =
  ( do
      ilevel' <- indentOfAtLeast ilevel
      name <- fieldSecName
      elementInLayoutContext (incIndentLevel ilevel') name
  )
    <|> ( do
            name <- fieldSecName
            elementInNonLayoutContext name
        )

-- An element (field or section) that is valid in a layout context.
-- In a layout context we can have fields and sections that themselves
-- either use layout style or that use braces style.
--
-- elementInLayoutContext ::= ':'  fieldLayoutOrBraces
--                          | arg* sectionLayoutOrBraces
elementInLayoutContext :: IndentLevel -> Name Position -> Parser (Field (WithComments Position))
elementInLayoutContext ilevel name =
  (do colon; fieldLayoutOrBraces ilevel name)
    <|> ( do
            args <- many sectionArg
            elems <- sectionLayoutOrBraces ilevel
            case elems of
              -- If there are no elements but comments, we attach them to the name (args can be multiple)
              Left' onlyCmts -> return (Section (WithComments onlyCmts <$> name) (noComments <$> args) [])
              Right' elems' -> return (Section (noComments name) (noComments <$> args) elems')
        )

-- An element (field or section) that is valid in a non-layout context.
-- In a non-layout context we can have only have fields and sections that
-- themselves use braces style, or inline style fields.
--
-- elementInNonLayoutContext ::= ':' FieldInlineOrBraces
--                             | arg* '\\n'? '{' elements '\\n'? '}'
elementInNonLayoutContext :: Name Position -> Parser (Field (WithComments Position))
elementInNonLayoutContext name =
  (do colon; noComments <$> fieldInlineOrBraces name) -- inline field or braces can never have comments
    <|> ( do
            args <- many sectionArg
            openBrace
            elems <- elements zeroIndentLevel
            optional tokIndent
            closeBrace

            case elems of
              Left' elementCmts -> return (Section (WithComments elementCmts <$> name) (noComments <$> args) [])
              Right' elems' -> return (Section (noComments name) (noComments <$> args) elems')
        )

-- The body of a field, using either layout style or braces style.
--
-- fieldLayoutOrBraces   ::= '\\n'? '{' comment* (content comment*)* '}'
--                         | comment* line? comment* ('\\n' line comment*)*
fieldLayoutOrBraces :: IndentLevel -> Name Position -> Parser (Field (WithComments Position))
fieldLayoutOrBraces ilevel name = braces <|> fieldLayout
  where
    braces :: Parser (Field (WithComments Position))
    braces = do
      openBrace
      preCmts <- many tokComment
      ls <- inLexerMode (LexerMode in_field_braces) (many $ commentsAfter fieldContent)
      closeBrace
      return $ Field (WithComments preCmts <$> name) ls

    fieldLayout :: Parser (Field (WithComments Position))
    fieldLayout = inLexerMode (LexerMode in_field_layout) $ do
      preCmts <- many tokComment
      l <- optionMaybe (commentsAfter fieldContent)
      ls <- many (do _ <- indentOfAtLeast ilevel; commentsAfter fieldContent)
      return
        ( case l of
            Nothing -> (Field (WithComments preCmts <$> name) ls)
            Just l' -> (Field (WithComments preCmts <$> name) (l' : ls))
        )

-- The body of a section, using either layout style or braces style.
--
-- sectionLayoutOrBraces ::= '\\n'? '{' elements \\n? '}'
--                         | elements
sectionLayoutOrBraces :: IndentLevel -> Parser (Either' [Comment Position] [Field (WithComments Position)])
sectionLayoutOrBraces ilevel =
  ( do
      openBrace
      elems <- elements zeroIndentLevel
      optional tokIndent
      closeBrace
      return elems
  )
    <|> (elements ilevel)

-- The body of a field, using either inline style or braces.
--
-- fieldInlineOrBraces   ::= '\\n'? '{' content '}'
--                         | content
fieldInlineOrBraces :: Name Position -> Parser (Field Position)
fieldInlineOrBraces name =
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
readFields = (fmap . map . fmap) unComments . readFieldsWithComments

-- | Like 'readFields' but also return lexer warnings.
readFields' :: B8.ByteString -> Either ParseError ([Field Position], [LexWarning])
readFields' = (fmap . Bi.first . map . fmap) unComments . readFieldsWithComments'

readFieldsWithComments :: B8.ByteString -> Either ParseError [Field (WithComments Position)]
readFieldsWithComments = fmap fst . readFieldsWithComments'

readFieldsWithComments' :: B8.ByteString -> Either ParseError ([Field (WithComments Position)], [LexWarning])
readFieldsWithComments' s = do
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
checkIndentation :: [Field (WithComments Position)] -> [LexWarning] -> [LexWarning]
checkIndentation [] = id
checkIndentation (Field name _ : fs') = checkIndentation' (unComments $ nameAnn name) fs'
checkIndentation (Section name _ fs : fs') = checkIndentation fs . checkIndentation' (unComments $ nameAnn name) fs'

-- | We compare adjacent fields to reduce the amount of reported indentation warnings.
checkIndentation' :: Position -> [Field (WithComments Position)] -> [LexWarning] -> [LexWarning]
checkIndentation' _ [] = id
checkIndentation' pos (Field name _ : fs') = checkIndentation'' pos (unComments $ nameAnn name) . checkIndentation' (unComments $ nameAnn name) fs'
checkIndentation' pos (Section name _ fs : fs') = checkIndentation'' pos (unComments $ nameAnn name) . checkIndentation fs . checkIndentation' (unComments $ nameAnn name) fs'

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

      Right x  -> print x
  where
    lexSt = mkLexState' . mkLexState

parseFile :: Show a => Parser a -> FilePath -> IO ()
parseFile p f = B8.readFile f >>= \s -> parseTest' p f s

parseStr  :: Show a => Parser a -> String -> IO ()
parseStr p = parseBS p . B8.pack

parseBS  :: Show a => Parser a -> B8.ByteString -> IO ()
parseBS p = parseTest' p "<input string>"
#endif

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

eof :: Parser ()
eof = notFollowedBy anyToken <?> "end of file"
  where
    notFollowedBy :: Parser LToken -> Parser ()
    notFollowedBy p =
      try
        ( (do L _ t <- try p; unexpected (describeToken t))
            <|> return ()
        )

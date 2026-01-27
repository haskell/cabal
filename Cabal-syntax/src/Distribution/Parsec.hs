{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Parsec
  ( Parsec (..)
  , ParsecParser (..)
  , runParsecParser
  , runParsecParser'
  , simpleParsec
  , simpleParsecBS
  , simpleParsec'
  , simpleParsecW'
  , lexemeParsec
  , eitherParsec
  , eitherTriviaParsec
  , explicitEitherParsec
  , explicitEitherParsec'

    -- * CabalParsing and diagnostics
  , CabalParsing (..)

    -- ** Warnings
  , PWarnType (..)
  , PWarning (..)
  , PWarningWithSource (..)
  , PSource (..)
  , showPWarning
  , showPWarningWithSource

    -- ** Errors
  , PError (..)
  , PErrorWithSource (..)
  , showPError
  , showPErrorWithSource

    -- * Position
  , Position (..)
  , incPos
  , retPos
  , showPos
  , zeroPos

    -- * Utilities
  , parsecToken
  , parsecToken'
  , parsecFilePath
  , parsecQuoted
  , parsecMaybeQuoted
  , parsecCommaList
  , triviaParsecCommaList
  , parsecCommaNonEmpty
  , parsecLeadingCommaList
  , triviaParsecLeadingCommaList
  , triviaParsecLeadingCommaListNonEmpty
  , triviaParsecCommaNonEmpty
  , triviaParsecLeadingOptCommaList
  , triviaParsecOptCommaList
  , parsecLeadingCommaNonEmpty
  , parsecOptCommaList
  , parsecLeadingOptCommaList
  , parsecStandard
  , parsecUnqualComponentName
  ) where

import Data.ByteString (ByteString)
import Data.Char (digitToInt, intToDigit)
import Data.List (transpose)
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Compat.CharParsing
import Distribution.Parsec.Error (PError (..), PErrorWithSource (..), showPError, showPErrorWithSource)

import qualified Data.Bifunctor as Bi
import Data.Monoid (Last (..))
import Distribution.Parsec.FieldLineStream (FieldLineStream, fieldLineStreamFromBS, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..), incPos, retPos, showPos, zeroPos)
import Distribution.Parsec.Warning
import Numeric (showIntAtBase)
import Prelude ()

import Distribution.Types.Annotation
import Distribution.Types.Namespace
import Debug.Pretty.Simple

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.DList as DList
import qualified Distribution.Compat.MonadFail as Fail
import qualified Text.Parsec as Parsec

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class for parsing with @parsec@. Mainly used for @.cabal@ file fields.
--
-- For parsing @.cabal@ like file structure, see "Distribution.Fields".
class Parsec a where
  {-# MINIMAL parsec | triviaParsec #-}
  parsec :: CabalParsing m => m a
  parsec = (\(_, y) -> y) <$> triviaParsec

  triviaParsec :: CabalParsing m => m (TriviaTree, a)
  triviaParsec = (\y -> (mempty, y)) <$> parsec

-- | Parsing class which
--
-- * can report Cabal parser warnings.
--
-- * knows @cabal-version@ we work with
class (P.CharParsing m, MonadPlus m, Fail.MonadFail m) => CabalParsing m where
  parsecWarning :: PWarnType -> String -> m ()

  parsecHaskellString :: m String
  parsecHaskellString = stringLiteral

  askCabalSpecVersion :: m CabalSpecVersion

-- | 'parsec' /could/ consume trailing spaces, this function /will/ consume.
lexemeParsec :: (CabalParsing m, Parsec a) => m a
lexemeParsec = parsec <* P.spaces

newtype ParsecParser a = PP
  { unPP
      :: CabalSpecVersion
      -> Parsec.Parsec FieldLineStream [PWarning] a
  }

liftParsec :: Parsec.Parsec FieldLineStream [PWarning] a -> ParsecParser a
liftParsec p = PP $ \_ -> p

instance Functor ParsecParser where
  fmap f p = PP $ \v -> fmap f (unPP p v)
  {-# INLINE fmap #-}

  x <$ p = PP $ \v -> x <$ unPP p v
  {-# INLINE (<$) #-}

instance Applicative ParsecParser where
  pure = liftParsec . pure
  {-# INLINE pure #-}

  f <*> x = PP $ \v -> unPP f v <*> unPP x v
  {-# INLINE (<*>) #-}
  f *> x = PP $ \v -> unPP f v *> unPP x v
  {-# INLINE (*>) #-}
  f <* x = PP $ \v -> unPP f v <* unPP x v
  {-# INLINE (<*) #-}

instance Alternative ParsecParser where
  empty = liftParsec empty

  a <|> b = PP $ \v -> unPP a v <|> unPP b v
  {-# INLINE (<|>) #-}

  many p = PP $ \v -> many (unPP p v)
  {-# INLINE many #-}

  some p = PP $ \v -> some (unPP p v)
  {-# INLINE some #-}

instance Monad ParsecParser where
  return = pure

  m >>= k = PP $ \v -> unPP m v >>= \x -> unPP (k x) v
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadPlus ParsecParser where
  mzero = empty
  mplus = (<|>)

instance Fail.MonadFail ParsecParser where
  fail = P.unexpected

instance P.Parsing ParsecParser where
  try p = PP $ \v -> P.try (unPP p v)
  p <?> d = PP $ \v -> unPP p v P.<?> d
  skipMany p = PP $ \v -> P.skipMany (unPP p v)
  skipSome p = PP $ \v -> P.skipSome (unPP p v)
  unexpected = liftParsec . P.unexpected
  eof = liftParsec P.eof
  notFollowedBy p = PP $ \v -> P.notFollowedBy (unPP p v)

instance P.CharParsing ParsecParser where
  satisfy = liftParsec . P.satisfy
  char = liftParsec . P.char
  notChar = liftParsec . P.notChar
  anyChar = liftParsec P.anyChar
  string = liftParsec . P.string

instance CabalParsing ParsecParser where
  parsecWarning t w = liftParsec $ do
    spos <- Parsec.getPosition
    Parsec.modifyState
      (PWarning t (Position (Parsec.sourceLine spos) (Parsec.sourceColumn spos)) w :)
  askCabalSpecVersion = PP pure

-- | Parse a 'String' with 'lexemeParsec'.
simpleParsec :: Parsec a => String -> Maybe a
simpleParsec =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Like 'simpleParsec' but for 'ByteString'
simpleParsecBS :: Parsec a => ByteString -> Maybe a
simpleParsecBS =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromBS

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
--
-- @since 3.4.0.0
simpleParsec' :: Parsec a => CabalSpecVersion -> String -> Maybe a
simpleParsec' spec =
  either (const Nothing) Just
    . runParsecParser' spec lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
-- Fail if there are any warnings.
--
-- @since 3.4.0.0
simpleParsecW' :: Parsec a => CabalSpecVersion -> String -> Maybe a
simpleParsecW' spec =
  either (const Nothing) (\(x, ws) -> if null ws then Just x else Nothing)
    . runParsecParser' spec ((,) <$> lexemeParsec <*> liftParsec Parsec.getState) "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec'.
eitherParsec :: Parsec a => String -> Either String a
eitherParsec = explicitEitherParsec parsec

-- | Parse a 'String' with 'lexemeParsec'
eitherTriviaParsec :: Parsec a => String -> Either String (TriviaTree, a)
eitherTriviaParsec = explicitEitherParsec triviaParsec

-- | Parse a 'String' with given 'ParsecParser'. Trailing whitespace is accepted.
explicitEitherParsec :: ParsecParser a -> String -> Either String a
explicitEitherParsec parser =
  either (Left . show) Right
    . runParsecParser (parser <* P.spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with given 'ParsecParser' and 'CabalSpecVersion'. Trailing whitespace is accepted.
-- See 'explicitEitherParsec'.
--
-- @since 3.4.0.0
explicitEitherParsec' :: CabalSpecVersion -> ParsecParser a -> String -> Either String a
explicitEitherParsec' spec parser =
  either (Left . show) Right
    . runParsecParser' spec (parser <* P.spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Run 'ParsecParser' with 'cabalSpecLatest'.
runParsecParser :: ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser = runParsecParser' cabalSpecLatest

-- | Like 'runParsecParser' but lets specify 'CabalSpecVersion' used.
--
-- @since 3.0.0.0
runParsecParser' :: CabalSpecVersion -> ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser' v p n = Parsec.runParser (unPP p v <* P.eof) [] n

instance Parsec a => Parsec (Identity a) where
  triviaParsec = (fmap . fmap) Identity triviaParsec

instance Parsec Bool where
  triviaParsec = P.munch1 isAlpha >>= fmap pure . postprocess
    where
      postprocess str
        | str == "True" = pure True
        | str == "False" = pure False
        | lstr == "true" = parsecWarning PWTBoolCase caseWarning *> pure True
        | lstr == "false" = parsecWarning PWTBoolCase caseWarning *> pure False
        | otherwise = fail $ "Not a boolean: " ++ str
        where
          lstr = map toLower str
          caseWarning =
            "Boolean values are case sensitive, use 'True' or 'False'."

instance Parsec a => Parsec (Last a) where
  triviaParsec = parsecLast

parsecLast :: (Parsec a, CabalParsing m) => m (TriviaTree, Last a)
parsecLast = ((fmap . fmap) (Last . Just) triviaParsec) <|> (pure . pure) mempty

-- | @[^ ,]@
parsecToken :: CabalParsing m => m String
parsecToken = parsecHaskellString <|> ((P.munch1 (\x -> not (isSpace x) && x /= ',') P.<?> "identifier") >>= checkNotDoubleDash)

-- | @[^ ]@
parsecToken' :: CabalParsing m => m String
parsecToken' = parsecHaskellString <|> ((P.munch1 (not . isSpace) P.<?> "token") >>= checkNotDoubleDash)

checkNotDoubleDash :: CabalParsing m => String -> m String
checkNotDoubleDash s = do
  when (s == "--") $
    parsecWarning PWTDoubleDash $
      unwords
        [ "Double-dash token found."
        , "Note: there are no end-of-line comments in .cabal files, only whole line comments."
        , "Use \"--\" (quoted double dash) to silence this warning, if you actually want -- token"
        ]

  return s

parsecFilePath :: CabalParsing m => m FilePath
parsecFilePath = parsecToken

-- | Parse a benchmark/test-suite types.
parsecStandard :: (CabalParsing m, Parsec ver) => (ver -> String -> a) -> m a
parsecStandard f = do
  cs <- some $ P.try (component <* P.char '-')
  ver <- parsec
  let name = map toLower (intercalate "-" cs)
  return $! f ver name
  where
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs

-- each component must contain an alphabetic character, to avoid
-- ambiguity in identifiers like foo-1 (the 1 is the version number).

triviaParsecCommaList :: (CabalParsing m, Namespace a) => m (TriviaTree, a) -> m [(TriviaTree, a)]
triviaParsecCommaList p = do
  xs <- toList <$> sepByNonEmpty' p' comma
  let xs' =
        map
          (\(n, (t, x)) -> (mark (SomeNamespace x) (TriviaTree [Nth n] mempty) <> t, x))
          (zip [1..] xs)
  pure xs'
  where
    -- TODO(leana8959):
    -- TriviaTree doesn't make sense when it's not in relationship with a data
    -- How do we represent this?
    comma :: CabalParsing m => m TriviaTree
    comma = do
      void $ P.string ","
      s <- spaces'
      pure $ TriviaTree [PostTrivia s] mempty

    p' = do -- p * spaces
      (t, x) <- p
      s <- spaces'
      let t' = mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

triviaParsecCommaListNE
  :: (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m (NonEmpty (TriviaTree, a))
triviaParsecCommaListNE p = sepByNonEmpty p comma
  where
    comma = (++) <$> P.string "," <*> spaces' P.<?> "comma"
    p' = do -- p * spaces
      (t, x) <- p
      s <- spaces'
      let t' = mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

parsecCommaList :: CabalParsing m => m a -> m [a]
parsecCommaList p = P.sepBy (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

parsecCommaNonEmpty :: CabalParsing m => m a -> m (NonEmpty a)
parsecCommaNonEmpty p = P.sepByNonEmpty (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

triviaParsecCommaNonEmpty
  :: (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m (NonEmpty (TriviaTree, a))
triviaParsecCommaNonEmpty p = do
  sepByNonEmpty' p' comma
  where
    p' = do -- p * spaces
      (t, x) <- p
      s <- spaces'
      let t' = mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

    comma :: CabalParsing m => m TriviaTree
    comma = do
      void $ P.string ","
      s <- spaces'
      pure $ TriviaTree [PostTrivia s] mempty

type SeparatorAnnotations a = [SeparatorAnnotation a]
data SeparatorAnnotation a
  = Leading a
  | Trailing a

-- |
-- Like 'sepEndByNonEmpty' but keeps the parsed separators.
-- Used by exact printing.
sepEndByNonEmpty'
  :: (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m TriviaTree -> m (NonEmpty (TriviaTree, a))
sepEndByNonEmpty' p sep = do
  u <- do
    (t, x) <- p
    s :: TriviaTree <- sep
    let t' = t <> mark (SomeNamespace x) s
    pure (t', x)

  us <- sepEndBy' p sep
  pure ( u :| us )


sepByNonEmpty'
  :: (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m TriviaTree -> m (NonEmpty (TriviaTree, a))
sepByNonEmpty' p sep = do
  -- p * (sep p)*
  u <- p
  us <- many $ do
      s <- sep
      (t, x) <- p
      let t' = t <> mark (SomeNamespace x) s
      pure (t', x)

  pure ( u :| us )

-- |
-- Like 'sepEndBy' but keeps the parsed separators.
-- Used by exact printing.
sepEndBy'
  :: (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m TriviaTree -> m [(TriviaTree, a)]
sepEndBy' p sep = sepEndBy1' p sep <|> pure []

sepEndBy1'
  :: (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m TriviaTree -> m [(TriviaTree, a)]
sepEndBy1' p sep = toList <$> sepEndByNonEmpty' p sep

-- | Like 'parsecCommaList' but accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- @
triviaParsecLeadingCommaList
  :: forall m a
   . (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m [(TriviaTree, a)]
triviaParsecLeadingCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> toList <$> sepEndByNonEmpty' p' comma <|> pure []
    Just _ -> toList <$> sepByNonEmpty' p' comma
  where
    p' :: (CabalParsing m, Namespace a) => m (TriviaTree, a)
    p' = do
      (t, x) <- p
      s <- spaces'
      let t' = t <> mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

    -- TODO(leana8959):
    -- TriviaTree doesn't make sense when it's not in relationship with a data
    -- How do we represent this?
    comma :: CabalParsing m => m TriviaTree
    comma = do
      c <- P.string ","
      s <- spaces'
      pure $ TriviaTree [PostTrivia (c <> s)] mempty

triviaParsecLeadingCommaListNonEmpty
  :: forall m a
   . (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m (NonEmpty (TriviaTree, a))
triviaParsecLeadingCommaListNonEmpty p = do
  c <- P.optional comma
  case c of
    Nothing -> sepEndByNonEmpty' p' comma
    Just _ -> sepByNonEmpty' p' comma
  where
    p' :: (CabalParsing m, Namespace a) => m (TriviaTree, a)
    p' = do
      (t, x) <- p
      s <- spaces'
      let t' = t <> mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

    -- TODO(leana8959):
    -- TriviaTree doesn't make sense when it's not in relationship with a data
    -- How do we represent this?
    comma :: CabalParsing m => m TriviaTree
    comma = do
      void $ P.string ","
      s <- spaces'
      pure $ TriviaTree [PostTrivia s] mempty

parsecLeadingCommaList :: CabalParsing m => m a -> m [a]
parsecLeadingCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> toList <$> P.sepEndByNonEmpty lp comma <|> pure []
    Just _ -> toList <$> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

-- |
--
-- @since 3.4.0.0
parsecLeadingCommaNonEmpty :: CabalParsing m => m a -> m (NonEmpty a)
parsecLeadingCommaNonEmpty p = do
  c <- P.optional comma
  case c of
    Nothing -> P.sepEndByNonEmpty lp comma
    Just _ -> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

parsecOptCommaList :: CabalParsing m => m a -> m [a]
parsecOptCommaList p = P.sepBy (p <* P.spaces) (P.optional comma)
  where
    comma = P.char ',' *> P.spaces

triviaParsecOptCommaList
  :: forall m a. (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m [(TriviaTree, a)]
triviaParsecOptCommaList p = P.sepBy p' (P.optional comma)
  where
    p' :: (CabalParsing m, Namespace a) => m (TriviaTree, a)
    p' = do
      (t, x) <- p
      s <- spaces'
      let t' = t <> mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

    comma = P.char ',' *> P.spaces P.<?> "comma"

-- | Like 'parsecOptCommaList' but
--
-- * require all or none commas
-- * accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- p*            -- no commas: many p
-- @
--
-- @since 3.0.0.0
parsecLeadingOptCommaList :: CabalParsing m => m a -> m [a]
parsecLeadingOptCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> sepEndBy1Start <|> pure []
    Just _ -> toList <$> P.sepByNonEmpty lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

    sepEndBy1Start = do
      x <- lp
      c <- P.optional comma
      case c of
        Nothing -> (x :) <$> many lp
        Just _ -> (x :) <$> P.sepEndBy lp comma

sepEndBy1Start'
  :: forall m a
   . (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m TriviaTree -> m [(TriviaTree, a)]
sepEndBy1Start' p comma = do
  x <- p
  c <- P.optional comma
  case c of
    Nothing -> (x :) <$> many p
    Just _ -> (x :) <$> P.sepEndBy p comma

triviaParsecLeadingOptCommaList
  :: forall m a
   . (CabalParsing m, Namespace a)
  => m (TriviaTree, a) -> m [(TriviaTree, a)]
triviaParsecLeadingOptCommaList p = do
  c <- P.optional comma
  case c of
    Nothing -> sepEndBy1Start' p' comma <|> pure []
    Just _ -> toList <$> sepByNonEmpty' p' comma
  where
    p' :: (CabalParsing m, Namespace a) => m (TriviaTree, a)
    p' = do
      (t, x) <- p
      s <- spaces'
      let t' = t <> mark (SomeNamespace x) (TriviaTree [PostTrivia s] mempty)
      pure (t', x)

    -- TODO(leana8959):
    -- TriviaTree doesn't make sense when it's not in relationship with a data
    -- How do we represent this?
    comma :: CabalParsing m => m TriviaTree
    comma = do
      void $ P.string ","
      s <- spaces'
      pure $ TriviaTree [PostTrivia s] mempty

-- | Content isn't unquoted
parsecQuoted :: CabalParsing m => m a -> m a
parsecQuoted = P.between (P.char '"') (P.char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted :: CabalParsing m => m a -> m a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecUnqualComponentName :: forall m. CabalParsing m => m String
parsecUnqualComponentName = state0 DList.empty
  where
    --
    -- using @kleene@ package we can easily see that
    -- we need only two states to recognize
    -- unqual-component-name
    --
    -- Compare with declarative
    -- 'Distribution.FieldGrammar.Described.reUnqualComponent'.
    --
    -- @
    -- import Kleene
    -- import Kleene.Internal.Pretty
    -- import Algebra.Lattice
    -- import Data.Char
    --
    -- import qualified Data.RangeSet.Map as RSet
    --
    -- main = do
    --     -- this is an approximation, to get an idea.
    --     let component :: RE Char
    --         component = star alphaNum <> alpha <> star alphaNum
    --
    --         alphaNum = alpha \/ num
    --         alpha    = unions $ map char ['a'..'z']
    --         num      = unions $ map char ['0'..'9']
    --
    --         re :: RE Char
    --         re = component <> star (char '-' <> component)
    --
    --     putPretty re
    --     putPretty $ fromTM re
    -- @

    state0 :: DList.DList Char -> m String
    state0 acc = do
      c <- ch -- <|> fail ("Invalid component, after " ++ DList.toList acc)
      case () of
        _
          | isDigit c -> state0 (DList.snoc acc c)
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> fail ("Empty component, after " ++ DList.toList acc)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    state1 :: DList.DList Char -> m String
    state1 acc = state1' acc `alt` return (DList.toList acc)

    state1' :: DList.DList Char -> m String
    state1' acc = do
      c <- ch
      case () of
        _
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> state0 (DList.snoc acc c)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    ch :: m Char
    !ch = P.satisfy (\c -> isAlphaNum c || c == '-')

    alt :: m String -> m String -> m String
    !alt = (<|>)

stringLiteral :: forall m. P.CharParsing m => m String
stringLiteral = lit
  where
    lit :: m String
    lit =
      foldr (maybe id (:)) ""
        <$> P.between (P.char '"') (P.char '"' P.<?> "end of string") (many stringChar)
        P.<?> "string"

    stringChar :: m (Maybe Char)
    stringChar =
      Just <$> stringLetter
        <|> stringEscape
        P.<?> "string character"

    stringLetter :: m Char
    stringLetter = P.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape :: m (Maybe Char)
    stringEscape = P.char '\\' *> esc
      where
        esc :: m (Maybe Char)
        esc =
          Nothing <$ escapeGap
            <|> Nothing <$ escapeEmpty
            <|> Just <$> escapeCode

    escapeEmpty, escapeGap :: m Char
    escapeEmpty = P.char '&'
    escapeGap = P.skipSpaces1 *> (P.char '\\' P.<?> "end of string gap")

escapeCode :: forall m. P.CharParsing m => m Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) P.<?> "escape code"
  where
    charControl, charNum :: m Char
    charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (P.char '^' *> (P.upper <|> P.char '@'))
    charNum = toEnum <$> num
      where
        num :: m Int
        num =
          bounded 10 maxchar
            <|> (P.char 'o' *> bounded 8 maxchar)
            <|> (P.char 'x' *> bounded 16 maxchar)
        maxchar = fromEnum (maxBound :: Char)

    bounded :: Int -> Int -> m Int
    bounded base bnd =
      foldl' (\x d -> base * x + digitToInt d) 0
        <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
      where
        thedigits :: [m Char]
        thedigits = map P.char ['0' .. '9'] ++ map P.oneOf (transpose [['A' .. 'F'], ['a' .. 'f']])

        toomuch :: m a
        toomuch = P.unexpected "out-of-range numeric escape sequence"

        bounded', bounded'' :: [m Char] -> [Int] -> m [Char]
        bounded' dps@(zero : _) bds =
          P.skipSome zero *> ([] <$ P.notFollowedBy (P.choice dps) <|> bounded'' dps bds)
            <|> bounded'' dps bds
        bounded' [] _ = error "bounded called with base 0"
        bounded'' dps [] = [] <$ P.notFollowedBy (P.choice dps) <|> toomuch
        bounded'' dps (bd : bds) =
          let anyd :: m Char
              anyd = P.choice dps

              nomore :: m ()
              nomore = P.notFollowedBy anyd <|> toomuch

              (low, ex, high) = case splitAt bd dps of
                (low', ex' : high') -> (low', ex', high')
                (_, _) -> error "escapeCode: Logic error"
           in ((:) <$> P.choice low <*> atMost (length bds) anyd) <* nomore
                <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                <|> if not (null bds)
                  then (:) <$> P.choice high <*> atMost (length bds - 1) anyd <* nomore
                  else empty
        atMost n p
          | n <= 0 = pure []
          | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

    charEsc :: m Char
    charEsc = P.choice $ parseEsc <$> escMap

    parseEsc (c, code) = code <$ P.char c
    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

    charAscii :: m Char
    charAscii = P.choice $ parseAscii <$> asciiMap

    parseAscii (asc, code) = P.try $ code <$ P.string asc
    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
    ascii2codes, ascii3codes :: [String]
    ascii2codes =
      [ "BS"
      , "HT"
      , "LF"
      , "VT"
      , "FF"
      , "CR"
      , "SO"
      , "SI"
      , "EM"
      , "FS"
      , "GS"
      , "RS"
      , "US"
      , "SP"
      ]
    ascii3codes =
      [ "NUL"
      , "SOH"
      , "STX"
      , "ETX"
      , "EOT"
      , "ENQ"
      , "ACK"
      , "BEL"
      , "DLE"
      , "DC1"
      , "DC2"
      , "DC3"
      , "DC4"
      , "NAK"
      , "SYN"
      , "ETB"
      , "CAN"
      , "SUB"
      , "ESC"
      , "DEL"
      ]
    ascii2, ascii3 :: String
    ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
    ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

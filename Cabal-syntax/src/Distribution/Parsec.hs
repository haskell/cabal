{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Parsec
  ( CabalParsec (..)
  , ParsecParser (..)
  , runParsecParser
  , runParsecParser'
  , simpleParsec
  , simpleParsecBS
  , simpleParsec'
  , simpleParsecW'
  , lexemeParsec
  , eitherParsec
  , explicitEitherParsec
  , explicitEitherParsec'
  , parsecHaskellString
  , askCabalSpecVersion

    -- ** Warnings
  , PWarnType (..)
  , PWarning (..)
  , showPWarning
  , parsecWarning

    -- ** Errors
  , PError (..)
  , showPError

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
  , parsecCommaNonEmpty
  , parsecLeadingCommaList
  , parsecLeadingCommaNonEmpty
  , parsecOptCommaList
  , parsecLeadingOptCommaList
  , parsecStandard
  , parsecUnqualComponentName

    -- * Parsers based on @parsers@ package
  , manyTill
  , eof
  , notChar
  , anyChar
  , option
  , skipOptional
  , endByNonEmpty
  , endBy
  , count
  , chainr
  , chainl
  , noneOf
  , newline
  , tab
  , lower
  , alphaNum
  , letter
  , digit
  , hexDigit
  , octDigit
  , satisfyRange
  , integral
  , signedIntegral
  , munch
  , unexpected
  , sepByNonEmpty
  , satisfy
  , char
  , (<?>)
  , munch1
  , string
  , skipSpaces1
  , sepBy
  , optional
  , try
  , space
  , spaces
  , between
  , choice
  , notFollowedBy
  ) where

import Control.Applicative (optional, (<**>))
import Data.ByteString (ByteString)
import Data.Char (digitToInt, intToDigit, isHexDigit, isLower, isOctDigit)
import Data.Foldable (asum)
import Data.List (transpose)
import qualified Data.List.NonEmpty as NE
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Parsec.Error (PError (..), showPError)
import Distribution.Parsec.FieldLineStream (FieldLineStream, fieldLineStreamFromBS, fieldLineStreamFromString)
import Distribution.Parsec.Position (Position (..), incPos, retPos, showPos, zeroPos)
import Distribution.Parsec.Warning (PWarnType (..), PWarning (..), showPWarning)
import Numeric (showIntAtBase)
import Prelude ()

import qualified Distribution.Compat.DList as DList
import qualified Distribution.Compat.MonadFail as Fail

import qualified Text.Parsec as Parsec

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class for parsing with @parsec@. Mainly used for @.cabal@ file fields.
--
-- For parsing @.cabal@ like file structure, see "Distribution.Fields".
class CabalParsec a where
  parsec :: ParsecParser a

parsecHaskellString :: ParsecParser String
parsecHaskellString = stringLiteral

-- | 'parsec' /could/ consume trailing spaces, this function /will/ consume.
lexemeParsec :: CabalParsec a => ParsecParser a
lexemeParsec = parsec <* spaces

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

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail
#endif

instance MonadPlus ParsecParser where
  mzero = empty
  mplus = (<|>)

instance Fail.MonadFail ParsecParser where
  fail = liftParsec . Parsec.unexpected

try :: ParsecParser a -> ParsecParser a
try p = PP $ \v -> Parsec.try (unPP p v)

(<?>) :: ParsecParser a -> String -> ParsecParser a
p <?> d = PP $ \v -> unPP p v Parsec.<?> d
infixr 0 <?>

skipMany :: ParsecParser a -> ParsecParser ()
skipMany p = PP $ \v -> Parsec.skipMany (unPP p v)

skipSome :: ParsecParser a -> ParsecParser ()
skipSome p = PP $ \v -> Parsec.skipMany1 (unPP p v)

unexpected :: String -> ParsecParser a
unexpected = liftParsec . Parsec.unexpected

eof :: ParsecParser ()
eof = liftParsec Parsec.eof

notFollowedBy :: Show a => ParsecParser a -> ParsecParser ()
notFollowedBy p = PP $ \v -> Parsec.notFollowedBy (unPP p v)

satisfy :: (Char -> Bool) -> ParsecParser Char
satisfy = liftParsec . Parsec.satisfy

char :: Char -> ParsecParser Char
char = liftParsec . Parsec.char

notChar :: Char -> ParsecParser Char
notChar c = liftParsec $ Parsec.satisfy (/= c)

anyChar :: ParsecParser Char
anyChar = liftParsec Parsec.anyChar

string :: String -> ParsecParser String
string = liftParsec . Parsec.string

parsecWarning :: PWarnType -> String -> ParsecParser ()
parsecWarning t w = liftParsec $ do
  spos <- Parsec.getPosition
  Parsec.modifyState
    (PWarning t (Position (Parsec.sourceLine spos) (Parsec.sourceColumn spos)) w :)

askCabalSpecVersion :: ParsecParser CabalSpecVersion
askCabalSpecVersion = PP pure

-- | Parse a 'String' with 'lexemeParsec'.
simpleParsec :: CabalParsec a => String -> Maybe a
simpleParsec =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Like 'simpleParsec' but for 'ByteString'
simpleParsecBS :: CabalParsec a => ByteString -> Maybe a
simpleParsecBS =
  either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromBS

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
--
-- @since 3.4.0.0
simpleParsec' :: CabalParsec a => CabalSpecVersion -> String -> Maybe a
simpleParsec' spec =
  either (const Nothing) Just
    . runParsecParser' spec lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec' using specific 'CabalSpecVersion'.
-- Fail if there are any warnings.
--
-- @since 3.4.0.0
simpleParsecW' :: CabalParsec a => CabalSpecVersion -> String -> Maybe a
simpleParsecW' spec =
  either (const Nothing) (\(x, ws) -> if null ws then Just x else Nothing)
    . runParsecParser' spec ((,) <$> lexemeParsec <*> liftParsec Parsec.getState) "<simpleParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with 'lexemeParsec'.
eitherParsec :: CabalParsec a => String -> Either String a
eitherParsec = explicitEitherParsec parsec

-- | Parse a 'String' with given 'ParsecParser'. Trailing whitespace is accepted.
explicitEitherParsec :: ParsecParser a -> String -> Either String a
explicitEitherParsec parser =
  either (Left . show) Right
    . runParsecParser (parser <* spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Parse a 'String' with given 'ParsecParser' and 'CabalSpecVersion'. Trailing whitespace is accepted.
-- See 'explicitEitherParsec'.
--
-- @since 3.4.0.0
explicitEitherParsec' :: CabalSpecVersion -> ParsecParser a -> String -> Either String a
explicitEitherParsec' spec parser =
  either (Left . show) Right
    . runParsecParser' spec (parser <* spaces) "<eitherParsec>"
    . fieldLineStreamFromString

-- | Run 'ParsecParser' with 'cabalSpecLatest'.
runParsecParser :: ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser = runParsecParser' cabalSpecLatest

-- | Like 'runParsecParser' but lets specify 'CabalSpecVersion' used.
--
-- @since 3.0.0.0
runParsecParser' :: CabalSpecVersion -> ParsecParser a -> FilePath -> FieldLineStream -> Either Parsec.ParseError a
runParsecParser' v p n = Parsec.runParser (unPP p v <* Parsec.eof) [] n

instance CabalParsec a => CabalParsec (Identity a) where
  parsec = Identity <$> parsec

instance CabalParsec Bool where
  parsec = munch1 isAlpha >>= postprocess
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

-- | @[^ ,]@
parsecToken :: ParsecParser String
parsecToken = parsecHaskellString <|> ((munch1 (\x -> not (isSpace x) && x /= ',') <?> "identifier") >>= checkNotDoubleDash)

-- | @[^ ]@
parsecToken' :: ParsecParser String
parsecToken' = parsecHaskellString <|> ((munch1 (not . isSpace) <?> "token") >>= checkNotDoubleDash)

checkNotDoubleDash :: String -> ParsecParser String
checkNotDoubleDash s = do
  when (s == "--") $
    parsecWarning PWTDoubleDash $
      unwords
        [ "Double-dash token found."
        , "Note: there are no end-of-line comments in .cabal files, only whole line comments."
        , "Use \"--\" (quoted double dash) to silence this warning, if you actually want -- token"
        ]

  return s

parsecFilePath :: ParsecParser FilePath
parsecFilePath = parsecToken

-- | Parse a benchmark/test-suite types.
parsecStandard :: CabalParsec ver => (ver -> String -> a) -> ParsecParser a
parsecStandard f = do
  cs <- some $ try (component <* char '-')
  ver <- parsec
  let name = map toLower (intercalate "-" cs)
  return $! f ver name
  where
    component = do
      cs <- munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs

-- each component must contain an alphabetic character, to avoid
-- ambiguity in identifiers like foo-1 (the 1 is the version number).

parsecCommaList :: ParsecParser a -> ParsecParser [a]
parsecCommaList p = sepBy (p <* spaces) (char ',' *> spaces <?> "comma")

parsecCommaNonEmpty :: ParsecParser a -> ParsecParser (NonEmpty a)
parsecCommaNonEmpty p = sepByNonEmpty (p <* spaces) (char ',' *> spaces <?> "comma")

-- | Like 'parsecCommaList' but accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- @
parsecLeadingCommaList :: ParsecParser a -> ParsecParser [a]
parsecLeadingCommaList p = do
  c <- optional comma
  case c of
    Nothing -> toList <$> sepEndByNonEmpty lp comma <|> pure []
    Just _ -> toList <$> sepByNonEmpty lp comma
  where
    lp = p <* spaces
    comma = char ',' *> spaces <?> "comma"

-- |
--
-- @since 3.4.0.0
parsecLeadingCommaNonEmpty :: ParsecParser a -> ParsecParser (NonEmpty a)
parsecLeadingCommaNonEmpty p = do
  c <- optional comma
  case c of
    Nothing -> sepEndByNonEmpty lp comma
    Just _ -> sepByNonEmpty lp comma
  where
    lp = p <* spaces
    comma = char ',' *> spaces <?> "comma"

parsecOptCommaList :: ParsecParser a -> ParsecParser [a]
parsecOptCommaList p = sepBy (p <* spaces) (optional comma)
  where
    comma = char ',' *> spaces

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
parsecLeadingOptCommaList :: ParsecParser a -> ParsecParser [a]
parsecLeadingOptCommaList p = do
  c <- optional comma
  case c of
    Nothing -> sepEndBy1Start <|> pure []
    Just _ -> toList <$> sepByNonEmpty lp comma
  where
    lp = p <* spaces
    comma = char ',' *> spaces <?> "comma"

    sepEndBy1Start = do
      x <- lp
      c <- optional comma
      case c of
        Nothing -> (x :) <$> many lp
        Just _ -> (x :) <$> sepEndBy lp comma

-- | Content isn't unquoted
parsecQuoted :: ParsecParser a -> ParsecParser a
parsecQuoted = between (char '"') (char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted :: ParsecParser a -> ParsecParser a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecUnqualComponentName :: ParsecParser String
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

    state0 :: DList.DList Char -> ParsecParser String
    state0 acc = do
      c <- ch -- <|> fail ("Invalid component, after " ++ DList.toList acc)
      case () of
        _
          | isDigit c -> state0 (DList.snoc acc c)
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> fail ("Empty component, after " ++ DList.toList acc)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    state1 :: DList.DList Char -> ParsecParser String
    state1 acc = state1' acc `alt` return (DList.toList acc)

    state1' :: DList.DList Char -> ParsecParser String
    state1' acc = do
      c <- ch
      case () of
        _
          | isAlphaNum c -> state1 (DList.snoc acc c)
          | c == '-' -> state0 (DList.snoc acc c)
          | otherwise -> fail ("Internal error, after " ++ DList.toList acc)

    ch :: ParsecParser Char
    !ch = satisfy (\c -> isAlphaNum c || c == '-')

    alt :: ParsecParser String -> ParsecParser String -> ParsecParser String
    !alt = (<|>)

stringLiteral :: ParsecParser String
stringLiteral = lit
  where
    lit :: ParsecParser String
    lit =
      foldr (maybe id (:)) ""
        <$> between (char '"') (char '"' <?> "end of string") (many stringChar)
        <?> "string"

    stringChar :: ParsecParser (Maybe Char)
    stringChar =
      Just <$> stringLetter
        <|> stringEscape
        <?> "string character"

    stringLetter :: ParsecParser Char
    stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape :: ParsecParser (Maybe Char)
    stringEscape = char '\\' *> esc
      where
        esc :: ParsecParser (Maybe Char)
        esc =
          Nothing <$ escapeGap
            <|> Nothing <$ escapeEmpty
            <|> Just <$> escapeCode

    escapeEmpty, escapeGap :: ParsecParser Char
    escapeEmpty = char '&'
    escapeGap = skipSpaces1 *> (char '\\' <?> "end of string gap")

escapeCode :: ParsecParser Char
escapeCode = (charEsc <|> charNum <|> charAscii <|> charControl) <?> "escape code"
  where
    charControl, charNum :: ParsecParser Char
    charControl = (\c -> toEnum (fromEnum c - fromEnum '@')) <$> (char '^' *> (upper <|> char '@'))
    charNum = toEnum <$> num
      where
        num :: ParsecParser Int
        num =
          bounded 10 maxchar
            <|> (char 'o' *> bounded 8 maxchar)
            <|> (char 'x' *> bounded 16 maxchar)
        maxchar = fromEnum (maxBound :: Char)

    bounded :: Int -> Int -> ParsecParser Int
    bounded base bnd =
      foldl' (\x d -> base * x + digitToInt d) 0
        <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
      where
        thedigits :: [ParsecParser Char]
        thedigits = map char ['0' .. '9'] ++ map oneOf (transpose [['A' .. 'F'], ['a' .. 'f']])

        toomuch :: ParsecParser a
        toomuch = unexpected "out-of-range numeric escape sequence"

        bounded', bounded'' :: [ParsecParser Char] -> [Int] -> ParsecParser [Char]
        bounded' dps@(zero : _) bds =
          skipSome zero *> ([] <$ notFollowedBy (choice dps) <|> bounded'' dps bds)
            <|> bounded'' dps bds
        bounded' [] _ = error "bounded called with base 0"
        bounded'' dps [] = [] <$ notFollowedBy (choice dps) <|> toomuch
        bounded'' dps (bd : bds) =
          let anyd :: ParsecParser Char
              anyd = choice dps

              nomore :: ParsecParser ()
              nomore = notFollowedBy anyd <|> toomuch

              (low, ex, high) = case splitAt bd dps of
                (low', ex' : high') -> (low', ex', high')
                (_, _) -> error "escapeCode: Logic error"
           in ((:) <$> choice low <*> atMost (length bds) anyd) <* nomore
                <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                <|> if not (null bds)
                  then (:) <$> choice high <*> atMost (length bds - 1) anyd <* nomore
                  else empty
        atMost n p
          | n <= 0 = pure []
          | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

    charEsc :: ParsecParser Char
    charEsc = choice $ parseEsc <$> escMap

    parseEsc (c, code) = code <$ char c
    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

    charAscii :: ParsecParser Char
    charAscii = choice $ parseAscii <$> asciiMap

    parseAscii (asc, code) = try $ code <$ string asc
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

-------------------------------------------------------------------------------
-- Parsers based on the @parsers@ package
-------------------------------------------------------------------------------

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- parser.
choice :: [ParsecParser a] -> ParsecParser a
choice = asum
{-# INLINE choice #-}

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- >  priority = option 0 (digitToInt <$> digit)
option :: a -> ParsecParser a -> ParsecParser a
option x p = p <|> pure x
{-# INLINE option #-}

-- | @skipOptional p@ tries to apply parser @p@.  It will parse @p@ or nothing.
-- It only fails if @p@ fails after consuming input. It discards the result
-- of @p@. (Plays the role of parsec's optional, which conflicts with Applicative's optional)
skipOptional :: ParsecParser a -> ParsecParser ()
skipOptional p = (() <$ p) <|> pure ()
{-# INLINE skipOptional #-}

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- >  braces  = between (symbol "{") (symbol "}")
between :: Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket
{-# INLINE between #-}

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- >  commaSep p  = p `sepBy` (symbol ",")
sepBy :: ParsecParser a -> ParsecParser sep -> ParsecParser [a]
sepBy p sep = toList <$> sepByNonEmpty p sep <|> pure []
{-# INLINE sepBy #-}

-- | @sepByNonEmpty p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a non-empty list of values returned by @p@.
sepByNonEmpty :: ParsecParser a -> ParsecParser sep -> ParsecParser (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)
{-# INLINE sepByNonEmpty #-}

-- | @sepEndByNonEmpty p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a non-empty list of values
-- returned by @p@.
sepEndByNonEmpty :: ParsecParser a -> ParsecParser sep -> ParsecParser (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, ie. haskell style
-- statements. Returns a list of values returned by @p@.
--
-- >  haskellStatements  = haskellStatement `sepEndBy` semi
sepEndBy :: ParsecParser a -> ParsecParser sep -> ParsecParser [a]
sepEndBy p sep = toList <$> sepEndByNonEmpty p sep <|> pure []
{-# INLINE sepEndBy #-}

-- | @endByNonEmpty p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a non-empty list of values returned by @p@.
endByNonEmpty :: ParsecParser a -> ParsecParser sep -> ParsecParser (NonEmpty a)
endByNonEmpty p sep = NE.some1 (p <* sep)
{-# INLINE endByNonEmpty #-}

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- >   cStatements  = cStatement `endBy` semi
endBy :: ParsecParser a -> ParsecParser sep -> ParsecParser [a]
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of
-- @n@ values returned by @p@.
count :: Applicative m => Int -> m a -> m [a]
count n p
  | n <= 0 = pure []
  | otherwise = sequenceA (replicate n p)
{-# INLINE count #-}

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are no occurrences of @p@, the value @x@ is
-- returned.
chainr :: ParsecParser a -> ParsecParser (a -> a -> a) -> a -> ParsecParser a
chainr p op x = chainr1 p op <|> pure x
{-# INLINE chainr #-}

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. If there are zero occurrences of @p@, the value @x@ is
-- returned.
chainl :: ParsecParser a -> ParsecParser (a -> a -> a) -> a -> ParsecParser a
chainl p op x = chainl1 p op <|> pure x
{-# INLINE chainl #-}

-- | @chainl1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@. . This parser can for example be used to eliminate left
-- recursion which typically occurs in expression grammars.
--
-- >  expr   = term   `chainl1` addop
-- >  term   = factor `chainl1` mulop
-- >  factor = parens expr <|> integer
-- >
-- >  mulop  = (*) <$ symbol "*"
-- >       <|> div <$ symbol "/"
-- >
-- >  addop  = (+) <$ symbol "+"
-- >       <|> (-) <$ symbol "-"
chainl1 :: ParsecParser a -> ParsecParser (a -> a -> a) -> ParsecParser a
chainl1 p op = scan
  where
    scan = p <**> rst
    rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}

-- | @chainr1 p op x@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned
-- by @p@.
chainr1 :: ParsecParser a -> ParsecParser (a -> a -> a) -> ParsecParser a
chainr1 p op = scan
  where
    scan = p <**> rst
    rst = (flip <$> op <*> scan) <|> pure id
{-# INLINE chainr1 #-}

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@.
-- This parser can be used to scan comments:
--
-- >  simpleComment   = do{ string "<!--"
-- >                      ; manyTill anyChar (try (string "-->"))
-- >                      }
--
--    Note the overlapping parsers @anyChar@ and @string \"-->\"@, and
--    therefore the use of the 'try' combinator.
manyTill :: ParsecParser a -> ParsecParser end -> ParsecParser [a]
manyTill p end = go where go = ([] <$ end) <|> ((:) <$> p <*> go)
{-# INLINE manyTill #-}

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"
oneOf :: [Char] -> ParsecParser Char
oneOf xs = liftParsec $ Parsec.satisfy (\c -> c `elem` xs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character is /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"
noneOf :: [Char] -> ParsecParser Char
noneOf xs = liftParsec $ Parsec.satisfy (\c -> c `notElem` xs)
{-# INLINE noneOf #-}

-- | Skips /zero/ or more white space characters. See also 'skipMany'.
spaces :: ParsecParser ()
spaces = skipMany space <?> "white space"
{-# INLINE spaces #-}

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.
space :: ParsecParser Char
space = satisfy isSpace <?> "space"
{-# INLINE space #-}

-- | Parses a newline character (\'\\n\'). Returns a newline character.
newline :: ParsecParser Char
newline = char '\n' <?> "new-line"
{-# INLINE newline #-}

-- | Parses a tab character (\'\\t\'). Returns a tab character.
tab :: ParsecParser Char
tab = char '\t' <?> "tab"
{-# INLINE tab #-}

-- | Parses an upper case letter. Returns the parsed character.
upper :: ParsecParser Char
upper = satisfy isUpper <?> "uppercase letter"
{-# INLINE upper #-}

-- | Parses a lower case character. Returns the parsed character.
lower :: ParsecParser Char
lower = satisfy isLower <?> "lowercase letter"
{-# INLINE lower #-}

-- | Parses a letter or digit. Returns the parsed character.
alphaNum :: ParsecParser Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
{-# INLINE alphaNum #-}

-- | Parses a letter (an upper case or lower case character). Returns the
-- parsed character.
letter :: ParsecParser Char
letter = satisfy isAlpha <?> "letter"
{-# INLINE letter #-}

-- | Parses a digit. Returns the parsed character.
digit :: ParsecParser Char
digit = satisfy isDigit <?> "digit"
{-# INLINE digit #-}

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character.
hexDigit :: ParsecParser Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigit #-}

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character.
octDigit :: ParsecParser Char
octDigit = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigit #-}

satisfyRange :: Char -> Char -> ParsecParser Char
satisfyRange a z = satisfy (\c -> c >= a && c <= z)
{-# INLINE satisfyRange #-}

-------------------------------------------------------------------------------
-- Our additions
-------------------------------------------------------------------------------

integral :: Integral a => ParsecParser a
integral = toNumber <$> some d <?> "integral"
  where
    toNumber = foldl' (\a b -> a * 10 + b) 0
    d = f <$> satisfyRange '0' '9'
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f _ = error "panic! integral"
{-# INLINE integral #-}

-- | Accepts negative (starting with @-@) and positive (without sign) integral
-- numbers.
--
-- @since 3.4.0.0
signedIntegral :: Integral a => ParsecParser a
signedIntegral = negate <$ char '-' <*> integral <|> integral
{-# INLINE signedIntegral #-}

-- | Greedily munch characters while predicate holds.
-- Require at least one character.
munch1 :: (Char -> Bool) -> ParsecParser String
munch1 = some . satisfy
{-# INLINE munch1 #-}

-- | Greedily munch characters while predicate holds.
-- Always succeeds.
munch :: (Char -> Bool) -> ParsecParser String
munch = many . satisfy
{-# INLINE munch #-}

skipSpaces1 :: ParsecParser ()
skipSpaces1 = skipSome space
{-# INLINE skipSpaces1 #-}

{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Parsec.Class (
    Parsec(..),
    ParsecParser (..),
    runParsecParser,
    simpleParsec,
    lexemeParsec,
    eitherParsec,
    -- * CabalParsing & warnings
    CabalParsing (..),
    PWarnType (..),
    -- * Utilities
    parsecToken,
    parsecToken',
    parsecFilePath,
    parsecQuoted,
    parsecMaybeQuoted,
    parsecCommaList,
    parsecLeadingCommaList,
    parsecOptCommaList,
    parsecStandard,
    parsecUnqualComponentName,
    ) where

import Data.Char                     (digitToInt, intToDigit)
import Data.Functor.Identity         (Identity (..))
import Data.List                     (transpose)
import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Distribution.Parsec.Common    (PWarnType (..), PWarning (..), Position (..))
import Numeric                       (showIntAtBase)
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.MonadFail   as Fail
import qualified Distribution.Compat.ReadP       as ReadP
import qualified Text.Parsec                     as Parsec

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Class for parsing with @parsec@. Mainly used for @.cabal@ file fields.
class Parsec a where
    parsec :: CabalParsing m => m a

-- | Parsing class which 
--
-- * can report Cabal parser warnings.
--
-- * knows @cabal-version@ we work with
--
class (P.CharParsing m, MonadPlus m) => CabalParsing m where
    parsecWarning :: PWarnType -> String -> m ()

    parsecHaskellString :: m String
    parsecHaskellString = stringLiteral

    askCabalSpecVersion :: m CabalSpecVersion

instance t ~ Char => CabalParsing (ReadP.Parser r t) where
    parsecWarning _ _   = pure ()
    askCabalSpecVersion = pure cabalSpecLatest

-- | 'parsec' /could/ consume trailing spaces, this function /will/ consume.
lexemeParsec :: (CabalParsing m, Parsec a) => m a
lexemeParsec = parsec <* P.spaces

newtype ParsecParser a = PP { unPP
    :: CabalSpecVersion -> Parsec.Parsec String [PWarning] a
    }

liftParsec :: Parsec.Parsec String [PWarning] a -> ParsecParser a
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
    f  *> x = PP $ \v -> unPP f v  *> unPP x v
    {-# INLINE (*>) #-}
    f <*  x = PP $ \v -> unPP f v <*  unPP x v
    {-# INLINE (<*) #-}

instance Alternative ParsecParser where
    empty = liftParsec empty

    a <|> b = PP $ \v -> unPP a v <|> unPP b v
    {-# INLINE (<|>) #-}

instance Monad ParsecParser where
    return = pure

    m >>= k = PP $ \v -> unPP m v >>= \x -> unPP (k x) v
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}

    fail = Fail.fail

instance MonadPlus ParsecParser where
    mzero = empty
    mplus = (<|>)

instance Fail.MonadFail ParsecParser where
    fail = P.unexpected

instance P.Parsing ParsecParser where
    try p           = PP $ \v -> P.try (unPP p v)
    p <?> d         = PP $ \v -> unPP p v P.<?> d
    skipMany p      = PP $ \v -> P.skipMany (unPP p v)
    skipSome p      = PP $ \v -> P.skipSome (unPP p v)
    unexpected      = liftParsec . P.unexpected
    eof             = liftParsec P.eof
    notFollowedBy p = PP $ \v -> P.notFollowedBy (unPP p v)

instance P.CharParsing ParsecParser where
    satisfy   = liftParsec . P.satisfy
    char      = liftParsec . P.char
    notChar   = liftParsec . P.notChar
    anyChar   = liftParsec P.anyChar
    string    = liftParsec . P.string

instance CabalParsing ParsecParser where
    parsecWarning t w = liftParsec $ Parsec.modifyState (PWarning t (Position 0 0) w :)
    askCabalSpecVersion = PP pure

-- | Parse a 'String' with 'lexemeParsec'.
simpleParsec :: Parsec a => String -> Maybe a
simpleParsec
    = either (const Nothing) Just . runParsecParser lexemeParsec "<simpleParsec>"

-- | Parse a 'String' with 'lexemeParsec'.
eitherParsec :: Parsec a => String -> Either String a
eitherParsec
    = either (Left . show) Right
    . runParsecParser lexemeParsec "<eitherParsec>"

-- | Run 'ParsecParser' with 'cabalSpecLatest'.
runParsecParser :: ParsecParser a -> FilePath -> String -> Either Parsec.ParseError a
runParsecParser p n = Parsec.runParser (unPP p cabalSpecLatest <* P.eof) [] n

instance Parsec a => Parsec (Identity a) where
    parsec = Identity <$> parsec

instance Parsec Bool where
    parsec = P.munch1 isAlpha >>= postprocess
      where
        postprocess str
            |  str == "True"  = pure True
            |  str == "False" = pure False
            | lstr == "true"  = parsecWarning PWTBoolCase caseWarning *> pure True
            | lstr == "false" = parsecWarning PWTBoolCase caseWarning *> pure False
            | otherwise       = fail $ "Not a boolean: " ++ str
          where
            lstr = map toLower str
            caseWarning =
                "Boolean values are case sensitive, use 'True' or 'False'."

-- | @[^ ,]@
parsecToken :: CabalParsing m => m String
parsecToken = parsecHaskellString <|> ((P.munch1 (\x -> not (isSpace x) && x /= ',')  P.<?> "identifier" ) >>= checkNotDoubleDash)

-- | @[^ ]@
parsecToken' :: CabalParsing m => m String
parsecToken' = parsecHaskellString <|> ((P.munch1 (not . isSpace) P.<?> "token") >>= checkNotDoubleDash)

checkNotDoubleDash ::  CabalParsing m => String -> m String
checkNotDoubleDash s = do
    when (s == "--") $ parsecWarning PWTDoubleDash $ unwords
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
    cs   <- some $ P.try (component <* P.char '-')
    ver  <- parsec
    let name = map toLower (intercalate "-" cs)
    return $! f ver name
  where
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs then fail "all digit component" else return cs
      -- each component must contain an alphabetic character, to avoid
      -- ambiguity in identifiers like foo-1 (the 1 is the version number).

parsecCommaList :: CabalParsing m => m a -> m [a]
parsecCommaList p = P.sepBy (p <* P.spaces) (P.char ',' *> P.spaces P.<?> "comma")

-- | Like 'parsecCommaList' but accept leading or trailing comma.
--
-- @
-- p (comma p)*  -- p `sepBy` comma
-- (comma p)*    -- leading comma
-- (p comma)*    -- trailing comma
-- @
parsecLeadingCommaList :: CabalParsing m => m a -> m [a]
parsecLeadingCommaList p = do
    c <- P.optional comma
    case c of
        Nothing -> P.sepEndBy1 lp comma <|> pure []
        Just _  -> P.sepBy1 lp comma
  where
    lp = p <* P.spaces
    comma = P.char ',' *> P.spaces P.<?> "comma"

parsecOptCommaList :: CabalParsing m => m a -> m [a]
parsecOptCommaList p = P.sepBy (p <* P.spaces) (P.optional comma)
  where
    comma = P.char ',' *>  P.spaces

-- | Content isn't unquoted
parsecQuoted :: CabalParsing m => m a -> m a
parsecQuoted = P.between (P.char '"') (P.char '"')

-- | @parsecMaybeQuoted p = 'parsecQuoted' p <|> p@.
parsecMaybeQuoted :: CabalParsing m => m a -> m a
parsecMaybeQuoted p = parsecQuoted p <|> p

parsecUnqualComponentName :: CabalParsing m => m String
parsecUnqualComponentName = intercalate "-" <$> P.sepBy1 component (P.char '-')
  where
    component :: CabalParsing m => m String
    component = do
      cs <- P.munch1 isAlphaNum
      if all isDigit cs
        then fail "all digits in portion of unqualified component name"
        else return cs

stringLiteral :: forall m. P.CharParsing m => m String
stringLiteral = lit where
    lit :: m String
    lit = foldr (maybe id (:)) ""
        <$> P.between (P.char '"') (P.char '"' P.<?> "end of string") (many stringChar)
        P.<?> "string"

    stringChar :: m (Maybe Char)
    stringChar = Just <$> stringLetter
         <|> stringEscape
         P.<?> "string character"

    stringLetter :: m Char
    stringLetter = P.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape :: m (Maybe Char)
    stringEscape = P.char '\\' *> esc where
        esc :: m (Maybe Char)
        esc = Nothing <$ escapeGap
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
      num = bounded 10 maxchar
        <|> (P.char 'o' *> bounded 8 maxchar)
        <|> (P.char 'x' *> bounded 16 maxchar)
      maxchar = fromEnum (maxBound :: Char)

  bounded :: Int -> Int -> m Int
  bounded base bnd = foldl' (\x d -> base * x + digitToInt d) 0
                 <$> bounded' (take base thedigits) (map digitToInt $ showIntAtBase base intToDigit bnd "")
    where
      thedigits :: [m Char]
      thedigits = map P.char ['0'..'9'] ++ map P.oneOf (transpose [['A'..'F'],['a'..'f']])

      toomuch :: m a
      toomuch = P.unexpected "out-of-range numeric escape sequence"

      bounded', bounded'' :: [m Char] -> [Int] -> m [Char]
      bounded' dps@(zero:_) bds = P.skipSome zero *> ([] <$ P.notFollowedBy (P.choice dps) <|> bounded'' dps bds)
                              <|> bounded'' dps bds
      bounded' []           _   = error "bounded called with base 0"
      bounded'' dps []         = [] <$ P.notFollowedBy (P.choice dps) <|> toomuch
      bounded'' dps (bd : bds) = let anyd :: m Char
                                     anyd = P.choice dps

                                     nomore :: m ()
                                     nomore = P.notFollowedBy anyd <|> toomuch

                                     (low, ex : high) = splitAt bd dps
                                  in ((:) <$> P.choice low <*> atMost (length bds) anyd) <* nomore
                                     <|> ((:) <$> ex <*> ([] <$ nomore <|> bounded'' dps bds))
                                     <|> if not (null bds)
                                            then (:) <$> P.choice high <*> atMost (length bds - 1) anyd <* nomore
                                            else empty
      atMost n p | n <= 0    = pure []
                 | otherwise = ((:) <$> p <*> atMost (n - 1) p) <|> pure []

  charEsc :: m Char
  charEsc = P.choice $ parseEsc <$> escMap

  parseEsc (c,code) = code <$ P.char c
  escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

  charAscii :: m Char
  charAscii = P.choice $ parseAscii <$> asciiMap

  parseAscii (asc,code) = P.try $ code <$ P.string asc
  asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)
  ascii2codes, ascii3codes :: [String]
  ascii2codes = [ "BS","HT","LF","VT","FF","CR","SO"
                , "SI","EM","FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK"
                ,"BEL","DLE","DC1","DC2","DC3","DC4","NAK"
                ,"SYN","ETB","CAN","SUB","ESC","DEL"]
  ascii2, ascii3 :: String
  ascii2 = "\BS\HT\LF\VT\FF\CR\SO\SI\EM\FS\GS\RS\US\SP"
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\BEL\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

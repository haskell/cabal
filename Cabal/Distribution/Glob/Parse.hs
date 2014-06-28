module Distribution.Glob.Parse where

import Control.Monad
    ( unless, liftM2 )
import Distribution.Compat.ReadP
import Distribution.Glob.Type

-- | We want to ensure this works the same way on all platforms, so we do not
-- use System.FilePath here.
--
-- Backslashes (like on Windows) may not be used as path separators, because
-- they would significantly complicate the implementation for little benefit.
pathSeparators :: [Char]
pathSeparators = "/"

charIsPathSeparator :: Char -> Bool
charIsPathSeparator x = x `elem` pathSeparators

-- | Characters which must not be parsed as literals if not escaped in glob
-- patterns
globSpecialChars :: [Char]
globSpecialChars = pathSeparators ++ "\\{}*[]?!^,"

isSpecialChar :: Char -> Bool
isSpecialChar x = x `elem` globSpecialChars

-- | Characters which can occur at the start of a bracket pattern to transform
-- it into its complement.
bracketComplementors :: [Char]
bracketComplementors = "^!"

isBracketComplementor :: Char -> Bool
isBracketComplementor x = x `elem` bracketComplementors

-- | Characters which must not be parsed as literals if not escaped in bracket
-- patterns.
bracketSpecialChars :: [Char]
bracketSpecialChars = bracketComplementors ++ "-[]\\/"

isBracketSpecialChar :: Char -> Bool
isBracketSpecialChar x = x `elem` bracketSpecialChars

-- | Like manyTill, but always consumes at least one occurence of 'p'.
manyTill1 :: ReadP r a -> ReadP [a] end -> ReadP r [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

-- | Parse an escape sequence. Anything is allowed, except a path separator.
escapeSequence :: ReadP r Char
escapeSequence = char '\\' >> satisfy (not . charIsPathSeparator)

parseLiteral :: ReadP r GlobPart
parseLiteral = fmap Literal $ manyTill1 literalSegment literalEnd
  where
  literalSegment = notSpecial +++ escapeSequence
  notSpecial = satisfy (not . isSpecialChar)
  literalEnd = do
    str <- look
    case str of
        (x:_) | isSpecialChar x -> return ()
        ""                      -> return ()
        _                       -> pfail

parsePathSeparator :: ReadP r GlobPart
parsePathSeparator = munch1 (== '/') >> return PathSeparator

parseCharList :: ReadP r GlobPart
parseCharList =
  between (char '[') (char ']')
    (fmap CharList (many1 parseCharListPart))

parseCharListComplement :: ReadP r GlobPart
parseCharListComplement =
  between (char '[') (char ']')
    (satisfy isBracketComplementor
     >> fmap CharListComplement (many1 parseCharListPart))

parseCharListPart :: ReadP r CharListPart
parseCharListPart = range <++ fmap CharLiteral literal
  where
  range = do
    start <- literal
    _ <- char '-'
    end <- literal
    unless (start < end) pfail
    return (Range start end)

  literal = satisfy (not . isBracketSpecialChar) +++ escapeSequence

parseWildOne :: ReadP r GlobPart
parseWildOne = char '?' >> return WildOne

-- | Parses either a WildMany or a WildManyRecursive.
parseWildMany :: ReadP r GlobPart
parseWildMany = do
  str <- munch1 (== '*')
  case str of
    "*"  -> return WildMany
    "**" -> return WildManyRecursive
    _    -> pfail

parseChoice :: ReadP r GlobPart
parseChoice =
  between (char '{') (char '}') $ do
    first <- parseGlobParts
    _ <- char ','
    rest <- sepBy1 (parseGlobParts <++ emptyGlob) (char ',')
    return (Choice (first : rest))
  where
  emptyGlob = return []

parseGlobPart :: ReadP r GlobPart
parseGlobPart = choice
  [ parseLiteral
  , parsePathSeparator
  , parseCharList
  , parseCharListComplement
  , parseWildOne
  , parseWildMany
  , parseChoice
  ]

parseGlobParts :: ReadP r [GlobPart]
parseGlobParts = many1 parseGlobPart

parseFileGlob :: String -> Maybe Glob
parseFileGlob fp =
  case fullyParsed (readP_to_S parseGlobParts fp) of
    [parts] -> Just (mkGlob parts)
    _       -> Nothing
  where
  fullyParsed = map fst . filter (null . snd)
  mkGlob parts =
    case sequence (map asLiteral parts) of
      Just literalParts -> NoGlob (concat literalParts)
      Nothing -> Glob (RealGlob parts)

  asLiteral (Literal str) = Just str
  asLiteral (PathSeparator) = Just "/"
  asLiteral _ = Nothing

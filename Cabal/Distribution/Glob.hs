module Distribution.Glob (
    parseGlob,
    --matchGlob, TODO
    GlobPart(..),
    Glob
)
    where

import Data.List
    ( intersperse )
import System.FilePath
    ( normalise, (</>), (<.>)
    , getSearchPath, takeDirectory, splitFileName
    , splitExtension, splitExtensions, splitDirectories )
import Distribution.Compat.ReadP

data GlobPart -- ^ A part of a glob.
    = Literal FilePath  -- ^ Match a file with this exact name. eg
                        -- "dictionary.txt", matches only "dictionary.txt"
    | Choice [Glob]     -- ^ Match exactly one of the given (literal) options.
                        -- eg "{a,b,c}" matches "a", "b", or "c"
    | MatchAny          -- ^ Match any part of a file or single directory name.
                        -- eg "jquery.*.js" matches "jquery.1.js",
                        -- "jquery.2.js", "jquery.3-pre.js"...
    | MatchAnyRecursive -- ^ Match any part of a file name, in the current
                        -- directory, and all subdirectories. eg "**/*Test.hs"
                        -- matches "GlobTest.hs", "test/HttpTest.hs",
                        -- "test/examples/ExampleTest.hs"...
    deriving (Show, Eq)

type Glob = [GlobPart] -- ^ A glob that can match any number of files.

-- Indicates whether a character needs escaping in glob patterns
isSpecialChar :: Char -> Bool
isSpecialChar x = x `elem` "\\{}*"

showGlobPart :: GlobPart -> FilePath
showGlobPart gp = case gp of
    (Literal name)    ->
        concatMap (\x -> if isSpecialChar x then ['\\', x] else [x]) name
    (Choice xs)       ->
        (\y -> "{" ++ y ++ "}" ) . concat . intersperse "," . map showGlob $ xs
    MatchAny          -> "*"
    MatchAnyRecursive -> "**"

showGlob :: Glob -> FilePath
showGlob = concatMap showGlobPart

parseLiteral :: ReadP r GlobPart
parseLiteral = fmap (Literal . concat) $ many1 literalSegment
    where
    literalSegment = unspecial +++ escapeSequence
    unspecial      = fmap (: []) $ satisfy (not . isSpecialChar)
    escapeSequence = fmap (: []) $ char '\\' >> satisfy isSpecialChar

parseChoice :: ReadP r GlobPart
parseChoice = do
    _ <- char '{'
    choices <- sepBy parseGlob' (char ',')
    _ <- char '}'
    return $ Choice choices

parseMatchAny :: ReadP r GlobPart
parseMatchAny = char '*' >> return MatchAny

parseMatchAnyRecursive :: ReadP r GlobPart
parseMatchAnyRecursive = string "**" >> return MatchAnyRecursive

parseGlobPart :: ReadP r GlobPart
parseGlobPart = choice
    [ parseLiteral
    , parseChoice
    , parseMatchAny
    , parseMatchAnyRecursive
    ]

parseGlob' :: ReadP r Glob
parseGlob' = many1 parseGlobPart

parseGlob :: FilePath -> Maybe Glob
parseGlob fp = case take 1 completeResults of
        (x:_) -> Just $ canonicalise x
        []    -> Nothing
    where
    results = readP_to_S parseGlob' fp
    completeResults = map fst $ filter ((== "") . snd) results

canonicalise :: Glob -> Glob
canonicalise = canonicaliseChoices . dedupBy joinLiterals . dedupBy joinMatchAnys
    where
    joinLiterals x = case x of
        (Literal a, Literal b) -> Just (Literal $ a ++ b)
        _ -> Nothing
    joinMatchAnys x = case x of
        (MatchAny, MatchAny) -> Just MatchAnyRecursive
        (MatchAny, MatchAnyRecursive) -> Just MatchAnyRecursive
        (MatchAnyRecursive, MatchAny) -> Just MatchAnyRecursive
        (MatchAnyRecursive, MatchAnyRecursive) -> Just MatchAnyRecursive
        _ -> Nothing
    canonicaliseChoices = fmap canonicaliseChoice
    canonicaliseChoice x = case x of
        Choice xs -> Choice $ fmap canonicalise xs
        y -> y

dedupBy :: ((a, a) -> Maybe a) -> [a] -> [a]
dedupBy f (x:y:ys) = case f (x, y) of
    Just z -> dedupBy f $ z : ys
    Nothing -> x : (dedupBy f $ y : ys)
dedupBy _ xs = xs

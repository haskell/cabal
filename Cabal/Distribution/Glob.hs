module Distribution.Glob (
    parseGlob,
    --matchGlob, TODO
    Glob(..),
)
    where

import Control.Applicative
    ( (<*>), (<$>) )
import Data.List
    ( intersperse )
import System.FilePath
    ( normalise, (</>), (<.>)
    , getSearchPath, takeDirectory, splitFileName
    , splitExtension, splitExtensions, splitDirectories )
import Distribution.Compat.ReadP

data Glob -- ^ A glob that can match any number of files.
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
    | Concat Glob Glob  -- ^ Matches one glob followed by another, eg "*.hs" is
                        -- Concat MatchAny (Literal ".hs")
 
showGlob :: Glob -> FilePath
showGlob glob = case glob of
    (Literal name)    -> name
    (Choice xs)       ->
        (\y -> "{" ++ y ++ "}" ) . concat . intersperse "," . map showGlob $ xs
    MatchAny          -> "*"
    MatchAnyRecursive -> "**"
    (Concat a b)      -> showGlob a ++ showGlob b

parseLiteral :: ReadP r Glob
parseLiteral = fmap (Literal . concat) $ many1 literalSegment
    where
    literalSegment = unspecial +++ escapeSequence
    unspecial      = satisfy (not . special) >>= return . (: [])
    escapeSequence = char '\\' >> satisfy special >>= (\x -> return ['\\', x])
    special x      = x `elem` "\\{}*"

parseChoice :: ReadP r Glob
parseChoice = do
    char '{'
    choices <- sepBy parseGlob (char ',')
    char '}'
    return $ Choice choices

parseMatchAny :: ReadP r Glob
parseMatchAny = char '*' >> return MatchAny

parseMatchAnyRecursive :: ReadP r Glob
parseMatchAnyRecursive = string "**" >> return MatchAnyRecursive

parseConcat :: ReadP r Glob
parseConcat = do
    a <- parseGlob
    b <- parseGlob
    return $ Concat a b

parseGlob :: ReadP r Glob
parseGlob = choice
    [ parseLiteral
    , parseChoice
    , parseMatchAny
    , parseMatchAnyRecursive
    , parseConcat
    ]

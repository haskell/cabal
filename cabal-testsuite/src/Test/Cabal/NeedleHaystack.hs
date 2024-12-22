{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for searching for a needle in a haystack, with transformations
-- for the strings to search in and the search strings such as re-encoding line
-- breaks or delimiting lines. Both LF and CRLF line breaks are recognized.
module Test.Cabal.NeedleHaystack
    ( TxContains(..)
    , txContainsId
    , NeedleHaystack(..)
    , symNeedleHaystack
    , multilineNeedleHaystack
    , needleHaystack
    , lineBreaksToSpaces
    , normalizePathSeparators
    , encodeLf
    , delimitLines
    ) where

import Prelude hiding (unlines)
import qualified Prelude (unlines)
import Data.List (tails)
import Data.Maybe (isJust)
import Distribution.System
import Data.List (isPrefixOf)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Network.URI (parseURI)

-- | Transformations for the search strings and the text to search in.
data TxContains =
    TxContains
        {
            -- | Reverse conversion for display, applied to the forward converted value.
            txBwd :: (String -> String),
            -- | Forward conversion for comparison.
            txFwd :: (String -> String)
        }

-- | Identity transformation for the search strings and the text to search in,
-- leaves them unchanged.
txContainsId :: TxContains
txContainsId = TxContains id id

-- | Conversions of the needle and haystack strings, the seach string and the
-- text to search in.
data NeedleHaystack =
    NeedleHaystack
        {
            expectNeedleInHaystack :: Bool,
            displayHaystack :: Bool,
            txNeedle :: TxContains,
            txHaystack :: TxContains
        }

-- | Symmetric needle and haystack functions, the same conversion for each going
-- forward and the same coversion for each going backward.
symNeedleHaystack :: (String -> String) -> (String -> String) -> NeedleHaystack
symNeedleHaystack bwd fwd = let tx = TxContains bwd fwd in NeedleHaystack True False tx tx

-- | Multiline needle and haystack functions with symmetric conversions. Going
-- forward converts line breaks to @"\\n"@.  Going backward adds visible
-- delimiters to lines.
multilineNeedleHaystack :: NeedleHaystack
multilineNeedleHaystack = symNeedleHaystack delimitLines encodeLf

-- | Minimal set up for finding the needle in the haystack. Doesn't change the
-- strings and doesn't displaying the haystack in any assertion failure message.
needleHaystack :: NeedleHaystack
needleHaystack = NeedleHaystack True False txContainsId txContainsId

-- | Replace line breaks with spaces, correctly handling @"\\r\\n"@.
--
-- >>> lineBreaksToSpaces "foo\nbar\r\nbaz"
-- "foo bar baz"
--
-- >>> lineBreaksToSpaces "foo\nbar\r\nbaz\n"
-- "foo bar baz"
--
-- >>> lineBreaksToSpaces "\nfoo\nbar\r\nbaz\n"
-- " foo bar baz"
lineBreaksToSpaces :: String -> String
lineBreaksToSpaces = unwords . lines . filter ((/=) '\r')

-- | Replaces path separators found with those of the current OS, URL-like paths
-- excluded.
--
-- > buildOS == Linux; normalizePathSeparators "foo\bar\baz" => "foo/bar/baz"
-- > buildOS == Windows; normalizePathSeparators "foo/bar/baz" => "foo\bar\baz"
normalizePathSeparators :: String -> String
normalizePathSeparators =
    unlines . map normalizePathSeparator . lines
    where
        normalizePathSeparator p =
            if | any (isJust . parseURI) (tails p) -> p
               | buildOS == Windows ->
                    [if Posix.isPathSeparator c then Windows.pathSeparator else c| c <- p]
               | otherwise ->
                    [if Windows.isPathSeparator c then Posix.pathSeparator else c| c <- p]

-- | @unlines@ from base will add a trailing newline if there isn't one already
-- but this one doesn't
--
-- >>> lines "abc"
-- ["abc"]
--
-- >>> Data.List.unlines $ lines "abc"
-- "abc\n"
--
-- >>> unlines $ lines "abc"
-- "abc"
unlines :: [String] -> String
unlines = maybe "" fst . unsnoc . Prelude.unlines

-- | @unsnoc@ is only in base >= 4.19 so we copy its definition here rather than
-- use CPP to conditionally import because we want to avoid CPP as that
-- interferes with string gaps in doctests.
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

-- | Replace line CRLF line breaks with LF line breaks.
--
-- >>> encodeLf "foo\nbar\r\nbaz"
-- "foo\nbar\nbaz"
--
-- >>> encodeLf "foo\nbar\r\nbaz\n"
-- "foo\nbar\nbaz\n"
--
-- >>> encodeLf "\nfoo\nbar\r\nbaz\n"
-- "\nfoo\nbar\nbaz\n"
--
-- >>> encodeLf "\n\n\n"
-- "\n\n\n"
encodeLf :: String -> String
encodeLf = filter (/= '\r')

-- | Mark lines with visible delimiters, @^@ at the start and @$@ at the end.
--
-- >>> delimitLines ""
-- "^$"
-- 
-- >>> delimitLines "\n"
-- "^$\n"
--
-- >>> delimitLines "\n\n"
-- "^$\n^$\n"
--
-- >>> delimitLines "\n\n\n"
-- "^$\n^$\n^$\n"
--
-- >>> delimitLines $ encodeLf "foo\nbar\r\nbaz"
-- "^foo$\n^bar$\n^baz$"
--
-- >>> delimitLines $ encodeLf "foo\nbar\r\nbaz\n"
-- "^foo$\n^bar$\n^baz$\n"
--
-- >>> delimitLines $ encodeLf "\nfoo\nbar\r\nbaz\n"
-- "^$\n^foo$\n^bar$\n^baz$\n"
delimitLines:: String -> String
delimitLines "" = "^$"
delimitLines "\n" = "^$\n"
delimitLines ('\n' : xs) = "^$\n" ++ delimitLines xs
delimitLines output = fixupStart . fixupEnd $
    foldr
            (\c acc -> c :
                if | "\n" == acc -> "$\n"
                   |("\n" `isPrefixOf` acc) -> "$\n^" ++ drop 1 acc
                   | otherwise -> acc
            )
            ""
    output
    where
        fixupStart :: String -> String
        fixupStart s@[] = s
        fixupStart s@('^' : _) = s
        fixupStart s = '^' : s

        fixupEnd :: String -> String
        fixupEnd s@[] = s
        fixupEnd s@(reverse -> '$' : _) = s
        fixupEnd s@(reverse -> '\n' : '$' : _) = s
        fixupEnd s = s ++ "$"

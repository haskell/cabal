{-# LANGUAGE MultiWayIf #-}

-- | Functions for searching for a needle in a haystack, with transformations
-- for the strings to search in and the search strings such as reencoding line
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
    , decodeLfMarkLines
    ) where

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
-- forward converts line breaks to @"\<EOL\>"@.  Going backward replaces
-- @"\<EOL\>"@ markers with line breaks and wrap lines with @^@ and @$@ markers.
multilineNeedleHaystack :: NeedleHaystack
multilineNeedleHaystack = symNeedleHaystack decodeLfMarkLines encodeLf

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
    -- WARNING: unlines will add a trailing newline if there isn't one already.
    --
    -- >>> lines "abc"
    -- ["abc"]
    --
    -- >>> unlines $ lines "abc"
    ---"abc\n"
    maybe "" fst . unsnoc . unlines . map normalizePathSeparator . lines
    where
        normalizePathSeparator p =
            if | any (isJust . parseURI) (tails p) -> p
               | buildOS == Windows ->
                    [if Posix.isPathSeparator c then Windows.pathSeparator else c| c <- p]
               | otherwise ->
                    [if Windows.isPathSeparator c then Posix.pathSeparator else c| c <- p]

        -- NOTE: unsnoc is only in base >= 4.19 so we copy its definition here
        -- rather than use CPP to conditionally import because we want to avoid
        -- CPP as that interferes with string gaps in doctests.
        unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

-- | Replace line breaks, be they @"\\r\\n"@ or @"\\n"@, with @"\<EOL\>"@.
--
-- >>> encodeLf "foo\nbar\r\nbaz"
-- "foo<EOL>bar<EOL>baz"
--
-- >>> encodeLf "foo\nbar\r\nbaz\n"
-- "foo<EOL>bar<EOL>baz"
--
-- >>> encodeLf "\nfoo\nbar\r\nbaz\n"
-- "<EOL>foo<EOL>bar<EOL>baz"
encodeLf :: String -> String
encodeLf =
    (\s -> if "<EOL>" `isPrefixOf` s then drop 5 s else s)
    . concat
    . (fmap ("<EOL>" ++))
    . lines
    . filter ((/=) '\r')

-- | Replace @"\<EOL\>"@ markers with @"\\n"@ line breaks and wrap lines with
-- @^@ and @$@ markers for the start and end.
--
-- >>> decodeLfMarkLines "foo<EOL>bar<EOL>baz"
-- "^foo$\n^bar$\n^baz$\n"
--
-- >>> decodeLfMarkLines "<EOL>foo<EOL>bar<EOL>baz"
-- "^foo$\n^bar$\n^baz$\n"
decodeLfMarkLines:: String -> String
decodeLfMarkLines output =
    (\xs -> case reverse $ lines xs of
        [] -> xs
        [line0] -> line0 ++ "$"
        lineN : ys ->
            let lineN' = lineN ++ "$"
            in unlines $ reverse (lineN' : ys))
    . unlines
    . (fmap ('^' :))
    . lines
    . (\s -> if "<EOL>" `isPrefixOf` s then drop 5 s else s)
    $ foldr
            (\c acc -> c :
                if ("<EOL>" `isPrefixOf` acc)
                    then "$\n" ++ drop 5 acc
                    else acc
            )
            ""
    output

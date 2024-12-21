module Test.Cabal.NeedleHaystack where

import Data.List (isPrefixOf)

-- | Transformations for the search strings and the text to search in.
data TxContains =
    TxContains
        {
            -- | Reverse conversion for display, applied to the forward converted value.
            txBwd :: (String -> String),
            -- | Forward conversion for comparison.
            txFwd :: (String -> String)
        }

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

multilineNeedleHaystack :: NeedleHaystack
multilineNeedleHaystack = symNeedleHaystack decodeLfMarkLines encodeLf

-- | Needle and haystack functions that do not change the strings. Set up for
-- finding the needle in the haystack and not displaying the line-delimited
-- haystack.
needleHaystack :: NeedleHaystack
needleHaystack = NeedleHaystack True False txContainsId txContainsId

-- | Replace line breaks with spaces, correctly handling "\r\n".
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

-- | Replace line breaks with <EOL>, correctly handling "\r\n".
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

-- | Replace <LF> markers with line breaks and wrap lines with ^ and $ markers
-- for the start and end.
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

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Functions for searching for a needle in a haystack, with transformations
-- for the strings to search in and the search strings such as re-encoding line
-- breaks or delimiting lines. Both LF and CRLF line breaks are recognized.
module Test.Cabal.NeedleHaystack
  ( TxFwdBwd (..)
  , txFwdBwdId
  , NeedleHaystack (..)
  , NeedleHaystackCompare
  , symNeedleHaystack
  , multilineNeedleHaystack
  , needleHaystack
  , lineBreaksToSpaces
  , normalizePathSeparators
  , encodeLf
  , delimitLines
  ) where

import Data.List (isPrefixOf, tails)
import Data.Maybe (isJust)
import Distribution.System
import Distribution.Utils.Generic (unsnoc)
import Network.URI (parseURI)
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Prelude hiding (unlines)
import qualified Prelude (unlines)

{-
Note [Multiline Needles]
~~~~~~~~~~~~~~~~~~~~~~~~

How we search for multiline strings in output that varies by platform.

Reading Expected Multiline Strings Verbatim
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With @ghc-9.12.1@ adding @-XMultilineStrings@, writing multiline string
expectations for @cabal-testsuite/PackageTests/**/*.test.hs@ test scripts might
be have been easier but for a catch. We run these tests with older @GHC@
versions so would need to use @-XCPP@ for those versions and the C preprocessor
does not play nicely with string gaps. While it is possible to encode a
multiline string as a single line with embedded LF characters or by breaking the
line up arbitrarily and using @++@ concatenation or by calling unlines on a list
of lines, string gaps are the multiline strings of Haskell prior to
@-XMultilineStrings@.

To avoid these problems and for the convenience of pasting the expected value
verbatim into a file, @readFileVerbatim@ can read the expected multiline output
for tests from a text file.  This has the same implementation as @readFile@ from
the @strict-io@ package to avoid problems at cleanup.

Warning: Windows file locking hack: hit the retry limit 3 while trying to remove
C:\Users\<username>\AppData\Local\Temp\cabal-testsuite-8376
cabal.test.hs:
C:\Users\<username>\AppData\Local\Temp\cabal-testsuite-8376\errors.expect.txt: removePathForcibly:DeleteFile
"\\\\?\\C:\\Users\\<username>\\AppData\\Local\\Temp\\cabal-testsuite-8376\\errors.expect.txt":
permission denied (The process cannot access the file because it is being used by another process.)

The other process accessing the file is @C:\WINDOWS\System32\svchost.exe@
running a @QueryDirectory@ event and this problem only occurs when the test
fails.

Hidden Actual Value Modification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The @assertOutputContains@ function was modifying the actual value (the test
output) with @concatOutput@ before checking if it contained the expected value.
This function, now renamed as @lineBreaksToSpaces@, would remove CR values and
convert LF values to spaces.

With this setup, false positives were possible. An expected value using string
gaps and spaces would match a @concatOutput@ modified actual value of
"foo_bar_baz", where '_' was any of space, LF or CRLF in the unmodified actual
value. The latter two are false positive matches.

> let expect = "foo \
>              \bar \
>              \baz"

False negatives were also possible. An expected value set up using string gaps
with LF characters or with @-XMultilineStrings@ wouldn't match an actual value
of "foo_bar_baz", where '_' was either LF or CRLF because these characters had
been replaced by spaces in the actual value, modified before the comparison.

> let expect = "foo\n\
>              \bar\n\
>              \baz"

> {-# LANGUAGE MultilineStrings #-}
>
> let expect = """
>              foo
>              bar
>              baz
>              """

We had these problems:

1. The actual value was changed before comparison and this change was not visible.
2. The expected value was not changed in the same way as the actual value. This
   made it possible for equal values to become unequal (false negatives) and for
   unequal values to become equal (false positives).

Explicit Changes and Visible Line Delimiters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To fix these problems, an added @assertOn@ function takes a @NeedleHaystack@
configuration for how the search is made, what to expect (to find the expected
value or not) and how to display the expected and actual values.

A pilcrow ¶ is often used to visibly display line endings but our terminal
output is restricted to ASCII so lines are delimited between @^@ and @$@
markers. The needle (the expected output fragment) is shown annotated this way
and the haystack (the actual output) can optionally be shown this way too.

This is still a lenient match, allowing LF to match CRLF, but @encodeLf@ doesn't
replace LF with spaces like @concatOutput@ (@lineBreaksToSpaces@) did:

If you choose to display the actual value by setting
@NeedleHaystack{displayHaystack = True}@ then its lines will be delimited.

With @assertOn@, supplying string transformation to both the needle and haystack
before comparison and before display can help find out why an expected value is
or isn't found in the test output.
-}

type NeedleHaystackCompare = String -> String -> Bool

-- | Transformations for the search strings and the text to search in.
data TxFwdBwd = TxFwdBwd
  { txBwd :: (String -> String)
  -- ^ Reverse conversion for display, applied to the forward converted value.
  , txFwd :: (String -> String)
  -- ^ Forward conversion for comparison.
  }

-- | Identity transformation for the search strings and the text to search in,
-- leaves them unchanged.
txFwdBwdId :: TxFwdBwd
txFwdBwdId = TxFwdBwd id id

-- | Conversions of the needle and haystack strings, the search string and the
-- text to search in.
data NeedleHaystack = NeedleHaystack
  { expectNeedleInHaystack :: Bool
  , displayHaystack :: Bool
  , txNeedle :: TxFwdBwd
  , txHaystack :: TxFwdBwd
  }

-- | Symmetric needle and haystack functions, the same conversion for each going
-- forward and the same conversion for each going backward.
symNeedleHaystack :: (String -> String) -> (String -> String) -> NeedleHaystack
symNeedleHaystack bwd fwd = let tx = TxFwdBwd bwd fwd in NeedleHaystack True False tx tx

-- | Multiline needle and haystack functions with symmetric conversions. Going
-- forward converts line breaks to @"\\n"@.  Going backward adds visible
-- delimiters to lines.
multilineNeedleHaystack :: NeedleHaystack
multilineNeedleHaystack = symNeedleHaystack delimitLines encodeLf

-- | Minimal set up for finding the needle in the haystack. Doesn't change the
-- strings and doesn't display the haystack in any assertion failure message.
needleHaystack :: NeedleHaystack
needleHaystack = NeedleHaystack True False txFwdBwdId txFwdBwdId

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
lineBreaksToSpaces = unwords . lines . filter ('\r' /=)

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
      if
          | any (isJust . parseURI) (tails p) -> p
          | buildOS == Windows ->
              [if Posix.isPathSeparator c then Windows.pathSeparator else c | c <- p]
          | otherwise ->
              [if Windows.isPathSeparator c then Posix.pathSeparator else c | c <- p]

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
delimitLines :: String -> String
delimitLines "" = "^$"
delimitLines "\n" = "^$\n"
delimitLines ('\n' : xs) = "^$\n" ++ delimitLines xs
delimitLines output =
  fixupStart . fixupEnd $
    foldr
      ( \c acc ->
          c
            : if
                | "\n" == acc -> "$\n"
                | ("\n" `isPrefixOf` acc) -> "$\n^" ++ drop 1 acc
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

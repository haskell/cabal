-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PreProcess.Unlit
-- Copyright   :  ...
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  portable
--
-- Remove the \"literal\" markups from a Haskell source file, including
-- \"@>@\", \"@\\begin{code}@\", \"@\\end{code}@\", and \"@#@\"

module Distribution.Simple.PreProcess.Unlit (unlit,plain) where

import Data.Char
import Data.List

-- | No unliteration.
plain :: String -> String -> String
plain _ hs = hs

-- | 'unlit' takes a filename (for error reports), and transforms the
--   given string, to eliminate the literate comments from the program text.
unlit :: FilePath -> String -> String
unlit file lhs = (unlines . classify file) (inlines lhs)


isBirdTrack = isPrefixOf ">"
isCpp = isPrefixOf "#"
isCodeStart = isPrefixOf "\\begin{code}"
isCodeEnd = isPrefixOf "\\end{code}"
isEmptyLine = all isSpace

-- Here we model a state machine, with each state represented by
-- a local function. We only have four states (well, five,
-- if you count the error state), but the rules
-- to transition between then are not so simple.
-- Would it be simpler to have more states?
--
-- Each state represents the type of line that was last read
-- i.e. are we in a comment section, or a latex-code section,
-- or a bird-code section, etc?
classify :: String -> [String] -> [String]
classify file [] = []
classify file lines = blank lines 1  -- begin in blank state
  where
    err n msg = error ("In file "++file++" at line "++show n++": "++msg++".")
    dropBird _ ('>':' ':x) = x
    dropBird _ ('>':x) = x
    dropBird n _ = err n "expecting '>' at start of line"
    mkComment x = "-- " ++ x

    transition classification [] _ _ = [classification]
    -- First case guarantee that xs is never null,
    -- so state functions can assume that too.
    transition classification xs n state = classification : state xs (n+1)

    latex (x:xs) n
      | isCodeEnd x = transition "" xs n comment
      | isCodeStart x = err n "\\begin{code} in code section"
      | otherwise = transition x xs n latex

    blank (x:xs) n
      | isCodeEnd x = err n "\\end{code} without \\begin{code}"
      | isCodeStart x = transition "" xs n latex
      | isCpp x = transition x xs n blank
      | isBirdTrack x = transition (dropBird n x) xs n bird
      | isEmptyLine x = transition "" xs n blank
      | otherwise = transition (mkComment x) xs n comment

    bird (x:xs) n
      | isCodeEnd x = err n "\\end{code} without \\begin{code}"
      | isCodeStart x = transition "" xs n latex
      | isCpp x = transition x xs n bird
      | isBirdTrack x = transition (dropBird n x) xs n bird
      | isEmptyLine x = transition "" xs n blank
      | otherwise = err n "program line before comment line"

    comment (x:xs) n
      | isCodeEnd x = err n "\\end{code} without \\begin{code}"
      | isCodeStart x = transition "" xs n latex
      | isCpp x = transition x xs n comment
      | isBirdTrack x = err n "comment line before program line"
      -- special case here: a truly empty line will terminate
      -- a comment section (and send us into the "blank" state)
      -- but a line containing whitespace will be treated as a
      -- comment (prefixed with "-- "), unless it is followed by
      -- a program line, in which case it is really blank.
      | null x = transition "" xs n blank
      | isEmptyLine x && null xs = transition (mkComment x) xs n comment
      | isEmptyLine x && isBirdTrack (head xs) = transition "" xs n blank
      | otherwise = transition (mkComment x) xs n comment


-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
inlines :: String -> [String]
inlines xs = lines' xs id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id    -- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id    -- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id    -- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))

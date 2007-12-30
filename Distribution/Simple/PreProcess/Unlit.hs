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

-- This version is interesting because instead of striping comment lines, it
-- turns them into "-- " style comments. This allows using haddock markup
-- in literate scripts without having to use "> --" prefix.

module Distribution.Simple.PreProcess.Unlit (unlit,plain) where

import Data.Char
import Data.List

data Classified = BirdTrack String | Blank String | Ordinary String
                | Line !Int String | CPP String
                | BeginCode | EndCode
                -- output only:
                | Error String | Comment String

-- | No unliteration.
plain :: String -> String -> String
plain _ hs = hs

classify :: String -> Classified
classify ('>':s) = BirdTrack s
classify ('#':s) = case tokens s of
                     (line:file:_) | all isDigit line
                                  && length file >= 2
                                  && head file == '"'
                                  && last file == '"'
                                -> Line (read line) (tail (init file))
                     _          -> CPP s
  where tokens = unfoldr $ \str -> case lex str of
                                   (t@(_:_), str'):_ -> Just (t, str')
                                   _                 -> Nothing
classify ('\\':s)
  | s `isPrefixOf` "begin{code}" = BeginCode
  | s `isPrefixOf` "end{code}"   = EndCode
classify s | all isSpace s       = Blank s
classify s                       = Ordinary s

unclassify :: Classified -> String
unclassify (BirdTrack s) = ' ':s
unclassify (Blank s)     = s
unclassify (Ordinary s)  = s
unclassify (Line n file) = "# " ++ show n ++ " " ++ show file
unclassify (CPP s)       = '#':s
unclassify (Comment s)   = "-- " ++ s

-- | 'unlit' takes a filename (for error reports), and transforms the
--   given string, to eliminate the literate comments from the program text.
unlit :: FilePath -> String -> Either String String
unlit file = either (Left . unlines
                          . map unclassify)
                     Right
           . checkErrors
           . reclassify
           . map classify
           . inlines

  where checkErrors ls = case [ e | Error e <- ls ] of
          []          -> Left  ls
          (message:_) -> Right (f ++ ":" ++ show n ++ ": " ++ message)
            where (f, n) = errorPos file 1 ls
        errorPos f n []              = (f, n)
        errorPos f n (Error _:_)     = (f, n)
        errorPos _ _ (Line n' f':ls) = errorPos f' n' ls
        errorPos f n (_         :ls) = errorPos f  (n+1) ls

-- Here we model a state machine, with each state represented by
-- a local function. We only have four states (well, five,
-- if you count the error state), but the rules
-- to transition between then are not so simple.
-- Would it be simpler to have more states?
--
-- Each state represents the type of line that was last read
-- i.e. are we in a comment section, or a latex-code section,
-- or a bird-code section, etc?
reclassify :: [Classified] -> [Classified]
reclassify = blank -- begin in blank state
  where
    latex []               = []
    latex (EndCode    :ls) = Blank "" : comment ls
    latex (BeginCode  :_ ) = [Error "\\begin{code} in code section"]
    latex (BirdTrack l:ls) = Ordinary ('>':l) : bird ls
    latex (          l:ls) = l : latex ls

    blank []               = []
    blank (EndCode    :_ ) = [Error "\\end{code} without \\begin{code}"]
    blank (BeginCode  :ls) = Blank ""    : latex ls
    blank (BirdTrack l:ls) = BirdTrack l : bird ls
    blank (Ordinary  l:ls) = Comment   l : comment ls
    blank (          l:ls) =           l : blank ls

    bird []              = []
    bird (EndCode   :_ ) = [Error "\\end{code} without \\begin{code}"]
    bird (BeginCode :ls) = Blank "" : latex ls
    bird (Blank l   :ls) = Blank l  : blank ls
    bird (Ordinary _:_ ) = [Error "program line before comment line"]
    bird (         l:ls) = l : bird ls

    comment []               = []
    comment (EndCode    :_ ) = [Error "\\end{code} without \\begin{code}"]
    comment (BeginCode  :ls) = Blank "" : latex ls
    comment (CPP l      :ls) = CPP l : comment ls
    comment (BirdTrack _:_ ) = [Error "comment line before program line"]
    -- special case here: a truly empty line will terminate
    -- a comment section (and send us into the "blank" state)
    comment (Blank ""   :ls) = Blank "" : blank ls
    -- but a line containing whitespace will be treated as a
    -- comment (prefixed with "-- "), unless it is followed by
    -- a program line, in which case it is really blank.
    comment (Blank     l:ls@(BirdTrack _:_)) = Blank l : blank ls
    comment (Blank     l:ls) = Comment l : comment ls
    comment (Line n f   :ls) = Line n f  : comment ls
    comment (Ordinary  l:ls) = Comment l : comment ls

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

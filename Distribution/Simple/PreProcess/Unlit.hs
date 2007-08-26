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
--
-- Part of the following code is from
-- /Report on the Programming Language Haskell/,
--   version 1.2, appendix C.

module Distribution.Simple.PreProcess.Unlit(unlit,plain) where

import Data.Char

data Classified = Program String | Blank | Comment
                | Include Int String | Pre String

plain :: String -> String -> String    -- no unliteration
plain _ hs = hs

classify :: [String] -> [Classified]
classify []                = []
classify ("\\begin{code}":rest) = Blank : allProg rest
   where allProg [] = []  -- Should give an error message,
                          -- but I have no good position information.
         allProg ("\\end{code}":xs) = Blank : classify xs
         allProg (x:xs) = Program x:allProg xs
classify (('>':x):xs)      = Program (' ':x) : classify xs
classify (('#':x):xs)      = (case words x of
                                (line:file:_) | all isDigit line
                                   -> Include (read line) file
                                _  -> Pre x
                             ) : classify xs
classify (x:xs) | all isSpace x = Blank:classify xs
classify (_:xs)                 = Comment:classify xs

unclassify :: Classified -> String
unclassify (Program s) = s
unclassify (Pre s)     = '#':s
unclassify (Include i f) = '#':' ':show i ++ ' ':f
unclassify Blank       = ""
unclassify Comment     = ""

-- | 'unlit' takes a filename (for error reports), and transforms the
--   given string, to eliminate the literate comments from the program text.
unlit :: FilePath -> String -> String
unlit file lhs = (unlines
                 . map unclassify
                 . adjacent file (0::Int) Blank
                 . classify) (inlines lhs)

-- Third argument is Comment, Blank or Program _
adjacent :: FilePath -> Int -> Classified -> [Classified] -> [Classified]
adjacent file n y             xs
 | file `seq` n `seq` y `seq` xs `seq` False = undefined
-- Include (# 123 "foo") lines are always OK and are treated as blank
-- The change our idea of filename and line number
adjacent _    _ _             (x@(Include i f):xs) = x: adjacent f    i     Blank xs
-- Other preprocessor lines (# ...) are always OK and are treated as blank
adjacent file n _             (x@(Pre _)      :xs) = x: adjacent file (n+1) Blank xs
-- Program and comment lines can't be adjacent
adjacent file n   (Program _) (  Comment      :_ ) = error (message file n "program" "comment")
adjacent file n   Comment     (  (Program _)  :_ ) = error (message file n "comment" "program")
-- Anything else is fine, and x is an allowable value for the third argument
adjacent file n _             (x              :xs) = x: adjacent file (n+1) x xs
adjacent _    _ _             []                   = []

message :: String -> Int -> String -> String -> String
message "\"\"" n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message []     n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message file   n p c = "In file " ++ file ++ " at line "++show n++": "++p++ " line before "++c++" line.\n"


-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
inlines :: String -> [String]
inlines xs = lines' xs id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id	-- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id	-- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id	-- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))


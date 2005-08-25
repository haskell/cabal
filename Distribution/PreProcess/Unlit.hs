-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PreProcess.Unlit
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

module Distribution.PreProcess.Unlit(unlit,plain) where

import Data.Char

-- exports:

unlit :: String -> String -> String
unlit file lhs = (unlines . map unclassify . adjacent file (0::Int) Blank
                 . classify 0) (tolines lhs)

plain :: String -> String -> String	-- no unliteration
plain _ hs = hs
----

data Classified = Program String | Blank | Comment
		| Include Int String | Pre String

classify :: Int -> [String] -> [Classified]
classify _ []                = []
classify _ (('\\':x):xs) | x == "begin{code}" = Blank : allProg xs
   where allProg [] = []	-- Should give an error message, but I have no
				-- good position information.
         allProg (('\\':x'):xs') |  x' == "end{code}" = Blank : classify 0 xs'
	 allProg (x':xs') = Program x':allProg xs'
classify 0 (('>':x):xs)  = let (sp,code) = span isSpace x in
                           Program code : classify (length sp + 1) xs
classify n (('>':x):xs)  = Program (drop (n-1) x) : classify n xs
classify _ (('#':x):xs)  =
     (case words x of
        (line:file:_) | all isDigit line -> Include (read line) file
        _                                -> Pre x
     ) : classify 0 xs
classify _ (x:xs) | all isSpace x = Blank:classify 0 xs
classify _ (_:xs)                 = Comment:classify 0 xs

unclassify :: Classified -> String
unclassify (Program s) = s
unclassify (Pre s)     = '#':s
unclassify (Include i f) = '#':' ':show i ++ ' ':f
unclassify Blank       = ""
unclassify Comment     = ""

adjacent :: String -> Int -> Classified -> [Classified] -> [Classified]
adjacent file 0 _             (x              :xs) = x: adjacent file 1 x xs
					-- force evaluation of line number
adjacent file n   (Program _) (Comment      :_) =
				error (message file n "program" "comment")
adjacent _    _ y@(Program _) (x@(Include i f):xs) = x: adjacent f    i     y xs
adjacent file n y@(Program _) (x@(Pre _)      :xs) = x: adjacent file (n+1) y xs
adjacent file n    Comment    ((Program _)  :_) =
				error (message file n "comment" "program")
adjacent _    _ y@Comment     (x@(Include i f):xs) = x: adjacent f    i     y xs
adjacent file n y@Comment     (x@(Pre _)      :xs) = x: adjacent file (n+1) y xs
adjacent _    _ y@Blank       (x@(Include i f):xs) = x: adjacent f    i     y xs
adjacent file n y@Blank       (x@(Pre _)      :xs) = x: adjacent file (n+1) y xs
adjacent file n _             (x              :xs) = x: adjacent file (n+1) x xs
adjacent _    _ _             []                    = []

message :: (Show a) => String -> a -> String -> String -> String
message "\"\"" n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message []     n p c = "Line "++show n++": "++p++ " line before "++c++" line.\n"
message file   n p c = "In file " ++ file ++ " at line "
                              ++show n++": "++p++ " line before "++c++" line.\n"


-- Re-implementation of 'lines', for better efficiency (but decreased
-- laziness).  Also, importantly, accepts non-standard DOS and Mac line
-- ending characters.
tolines :: String -> [String]
tolines s' = lines' s' id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id      -- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id      -- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id      -- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))



{-
-- A very naive version of unliteration....
module Unlit(unlit) where
-- This version does not handle \begin{code} & \end{code}, and it is
-- careless with indentation.
unlit = map unlitline

unlitline ('>' : s) = s
unlitline _ = ""
-}


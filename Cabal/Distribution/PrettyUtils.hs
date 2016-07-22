-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PrettyUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for pretty printing.
{-# OPTIONS_HADDOCK hide #-}
module Distribution.PrettyUtils (
    Separator,
    -- * Internal
    showFilePath,
    showToken,
    showTestedWith,
    showFreeText,
    indentWith,
    ) where

import Distribution.Compiler (CompilerFlavor)
import Distribution.Version  (VersionRange)

import Data.Char             (isSpace)
import Distribution.Text     (disp)
import Text.PrettyPrint      (Doc, empty, text, vcat, (<+>))

type Separator = ([Doc] -> Doc)

showFilePath :: FilePath -> Doc
showFilePath "" = empty
showFilePath x  = showToken x

showToken :: String -> Doc
showToken str
    | not (any dodgy str) && not (null str)  = text str
    | otherwise                              = text (show str)
  where
    dodgy c = isSpace c || c == ','

showTestedWith :: (CompilerFlavor, VersionRange) -> Doc
showTestedWith (compiler, vr) = text (show compiler) <+> disp vr

-- | Pretty-print free-format text, ensuring that it is vertically aligned,
-- and with blank lines replaced by dots for correct re-parsing.
showFreeText :: String -> Doc
showFreeText "" = empty
showFreeText s  = vcat [text (if null l then "." else l) | l <- lines_ s]

-- | 'lines_' breaks a string up into a list of strings at newline
-- characters.  The resulting strings do not contain newlines.
lines_                   :: String -> [String]
lines_ []                =  [""]
lines_ s                 =  let (l, s') = break (== '\n') s
                            in  l : case s' of
                                        []    -> []
                                        (_:s'') -> lines_ s''

-- | the indentation used for pretty printing
indentWith :: Int
indentWith = 4

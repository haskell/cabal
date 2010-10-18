-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Text
-- Copyright   :  Duncan Coutts 2007
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines a 'Text' class which is a bit like the 'Read' and 'Show'
-- classes. The difference is that is uses a modern pretty printer and parser
-- system and the format is not expected to be Haskell concrete syntax but
-- rather the external human readable representation used by Cabal.
--
module Distribution.Text (
  Text(..),
  display,
  simpleParse,
  ) where

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp

import Data.Version (Version(Version))
import qualified Data.Char as Char (isDigit, isAlphaNum, isSpace)

class Text a where
  disp  :: a -> Disp.Doc
  parse :: Parse.ReadP r a

display :: Text a => a -> String
display = Disp.renderStyle style . disp
  where style = Disp.Style {
          Disp.mode            = Disp.PageMode,
          Disp.lineLength      = 79,
          Disp.ribbonsPerLine  = 1.0
        }

simpleParse :: Text a => String -> Maybe a
simpleParse str = case [ p | (p, s) <- Parse.readP_to_S parse str
                       , all Char.isSpace s ] of
  []    -> Nothing
  (p:_) -> Just p

-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Text Bool where
  disp  = Disp.text . show
  parse = Parse.choice [ (Parse.string "True" Parse.+++
                          Parse.string "true") >> return True
                       , (Parse.string "False" Parse.+++
                          Parse.string "false") >> return False ]

instance Text Version where
  disp (Version branch _tags)     -- Death to version tags!!
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))

  parse = do
      branch <- Parse.sepBy1 digits (Parse.char '.')
      tags   <- Parse.many (Parse.char '-' >> Parse.munch1 Char.isAlphaNum)
      return (Version branch tags)  --TODO: should we ignore the tags?
    where
      digits = do
        first <- Parse.satisfy Char.isDigit
        if first == '0'
          then return 0
          else do rest <- Parse.munch Char.isDigit
                  return (read (first : rest))

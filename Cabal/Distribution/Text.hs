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
  render,
  brokenString
  ) where

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp

import Data.Version (Version(Version))
import qualified Data.Char as Char (isDigit, isAlphaNum, isSpace)

class Text a where
  disp  :: a -> Disp.Doc
  parse :: Parse.ReadP r a

-- | Display a 'Text' value with the Cabal default style.
display :: Text a => a -> String
display = Disp.renderStyle defaultStyle . disp

-- | similar to Disp.render, but using the Cabal default style
--   (which is different from Text.Prettyprint default).
render :: Disp.Doc -> String
render = Disp.renderStyle defaultStyle

-- | Takes a string, and turns it into a paragraph-like
--   Doc, i.e. an fsep of the words in it. Main purpose is
--   to produce indented paragraphs.
brokenString :: String -> Disp.Doc
brokenString s = Disp.fsep $ fmap Disp.text $ words s

defaultStyle :: Disp.Style
defaultStyle = Disp.Style
  { Disp.mode            = Disp.PageMode
  , Disp.lineLength      = 79  -- Disp default: 100
  , Disp.ribbonsPerLine  = 1.0 -- Disp default: 1.5
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

instance Text Int where
  disp  = Disp.text . show
  parse = (fmap negate $ Parse.char '-' >> parseNat) Parse.+++ parseNat

-- | Parser for non-negative integers.
parseNat :: Parse.ReadP r Int
parseNat = read `fmap` Parse.munch1 Char.isDigit

instance Text Version where
  disp (Version branch _tags)     -- Death to version tags!!
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))

  parse = do
      branch <- Parse.sepBy1 parseNat (Parse.char '.')
                -- allow but ignore tags:
      _tags  <- Parse.many (Parse.char '-' >> Parse.munch1 Char.isAlphaNum)
      return (Version branch [])

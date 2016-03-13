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
  defaultStyle,
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

-- | The default rendering style used in Cabal for console output.
defaultStyle :: Disp.Style
defaultStyle = Disp.Style { Disp.mode           = Disp.PageMode
                          , Disp.lineLength     = 79
                          , Disp.ribbonsPerLine = 1.0
                          }

display :: Text a => a -> String
display = Disp.renderStyle defaultStyle . disp

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

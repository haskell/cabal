module Distribution.Text (
  Text(..),
  display,
  simpleParse,
  ) where

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp

import qualified Data.Char as Char (isSpace)

class Text a where
  disp  :: a -> Disp.Doc
  parse :: Parse.ReadP r a

display :: Text a => a -> String
display = Disp.render . disp

simpleParse :: Text a => String -> Maybe a
simpleParse str = case [ p | (p, s) <- Parse.readP_to_S parse str
                       , all Char.isSpace s ] of
  []    -> Nothing
  (p:_) -> Just p

-- -----------------------------------------------------------------------------
-- Instances for types from the base package

instance Text Bool where
  disp  = Disp.text . show
  parse = Parse.choice [ (Parse.string "true" Parse.<++
                          Parse.string "True") >> return True
                       , (Parse.string "false" Parse.<++
                          Parse.string "False") >> return False ]

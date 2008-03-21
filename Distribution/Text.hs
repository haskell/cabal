module Distribution.Text (
  Text(..),
  display,
  simpleParse,
  ) where

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import Text.PrettyPrint ((<>))

import Data.Version (Version(Version))
import qualified Data.Char as Char (isDigit, isAlphaNum, isSpace)

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

instance Text Version where
  disp (Version branch tags)
    = Disp.hcat (Disp.punctuate (Disp.char '.') (map Disp.int branch))
   <> Disp.hcat (map (\tag -> Disp.char '-' <> Disp.text tag) tags)

  parse = do
      branch <- Parse.sepBy1 digits (Parse.char '.')
      tags   <- Parse.many (Parse.char '-' >> Parse.munch1 Char.isAlphaNum)
      return (Version branch tags)
    where
      digits = do
        first <- Parse.satisfy Char.isDigit
        if first == '0'
          then return 0
          else do rest <- Parse.munch Char.isDigit
                  return (read (first : rest))

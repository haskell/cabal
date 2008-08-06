module Distribution.Client.ParseUtils (
    parseBasicStanza,
    showFields,
  ) where

import Distribution.ParseUtils 
         ( Field(..), FieldDescr(..), ParseResult(..)
         , readFields, warning, lineNo )
 
import Control.Monad (foldM)
import Data.Maybe (listToMaybe)
import Text.PrettyPrint.HughesPJ (render, vcat, text, (<>), (<+>))

parseBasicStanza :: [FieldDescr a] -> a -> String -> ParseResult a
parseBasicStanza fields empty inp = 
    readFields inp >>= foldM (setField fields) empty

setField :: [FieldDescr a]
         -> a
         -> Field
         -> ParseResult a
setField fs x (F line f val) =
    case lookupFieldDescr fs f of
      Nothing -> 
          do warning ("Unrecognized field " ++ f ++ " on line " ++ show line)
             return x
      Just (FieldDescr _ _ set) -> set line val x
setField _ x s = 
    do warning ("Unrecognized stanza on line " ++ show (lineNo s))
       return x

lookupFieldDescr :: [FieldDescr a] -> String -> Maybe (FieldDescr a)
lookupFieldDescr fs n = listToMaybe [f | f@(FieldDescr name _ _) <- fs, name == n]

showFields :: [FieldDescr a] -> a -> String
showFields fields x = render $ vcat [ text name <> text ":" <+> getter x
                                    | FieldDescr name getter _ <- fields ]

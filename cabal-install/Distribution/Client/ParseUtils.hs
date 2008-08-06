--FIXME: make this whole module go away!
module Distribution.Client.ParseUtils (
    parseFields
  ) where

import Distribution.ParseUtils
         ( Field(..), FieldDescr(..), ParseResult(..)
         , readFields, warning, lineNo )

import Control.Monad (foldM)
import qualified Data.Map as Map

--FIXME: this function is now in Cabal as of 1.5, so remove this local copy
parseFields :: [FieldDescr a] -> a -> String -> ParseResult a
parseFields fields initial = \str ->
  readFields str >>= foldM setField initial
  where
    fieldMap = Map.fromList
      [ (name, f) | f@(FieldDescr name _ _) <- fields ]
    setField accum (F line name value) = case Map.lookup name fieldMap of
      Just (FieldDescr _ _ set) -> set line value accum
      Nothing -> do
        warning $ "Unrecognized field " ++ name ++ " on line " ++ show line
        return accum
    setField accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExtraSource
  ( ExtraSource (..)
  , extraSourceFromPath
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Path (FileOrDir (..), Pkg, SymbolicPath)

import qualified Distribution.Compat.CharParsing as P
import Distribution.FieldGrammar.Newtypes (SymbolicPathNT (..))
import qualified Text.PrettyPrint as PP

data ExtraSource = ExtraSource
  { extraSourceFile :: SymbolicPath Pkg File
  , extraSourceOpts :: [String]
  }
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary ExtraSource
instance Structured ExtraSource
instance NFData ExtraSource where rnf = genericRnf

instance Parsec ExtraSource where
  parsec = do
    SymbolicPathNT path <- parsec <* P.spaces
    opts <- P.optional (parensLax (P.sepBy p P.spaces))
    return (ExtraSource path (fromMaybe mempty opts))
    where
      p :: P.CharParsing p => p String
      p = some $ P.satisfy (\c -> not (isSpace c) && not (c == ')'))

parensLax :: P.CharParsing m => m a -> m a
parensLax p = P.between (P.char '(' *> P.spaces) (P.char ')' *> P.spaces) p

instance Pretty ExtraSource where
  pretty (ExtraSource path opts) =
    pretty (SymbolicPathNT path) <<>> PP.parens (PP.hsep (map PP.text opts))

extraSourceFromPath :: SymbolicPath Pkg File -> ExtraSource
extraSourceFromPath fp = ExtraSource fp mempty

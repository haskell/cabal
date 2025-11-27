{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Imports where

import Distribution.Compat.Prelude

data WithImports a = WithImports
  { getImportNames :: ![ImportName]
  , unImportNames :: !a
  }
  deriving (Show, Functor, Eq, Ord, Read, Data, Generic)

instance Binary a => Binary (WithImports a)
instance Structured a => Structured (WithImports a)
instance NFData a => NFData (WithImports a) where rnf = genericRnf

type ImportName = String

mapImports :: ([ImportName] -> [ImportName]) -> WithImports a -> WithImports a
mapImports f (WithImports imports x) = WithImports (f imports) x

noImports :: a -> WithImports a
noImports = WithImports mempty

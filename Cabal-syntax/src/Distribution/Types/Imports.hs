{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Distribution.Types.Imports where

data WithImports a = WithImports
  { getImportNames :: ![ImportName]
  , unImportNames :: !a
  }
  deriving (Show, Functor)

type ImportName = String

mapImports :: ([ImportName] -> [ImportName]) -> WithImports a -> WithImports a
mapImports f (WithImports imports x) = WithImports (f imports) x

noImports :: a -> WithImports a
noImports = WithImports mempty

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Distribution.Types.Imports where

data WithImports a = WithImports
  { getImportNames :: ![String]
  , unImportNames :: !a
  }
  deriving (Functor)

noImports :: a -> WithImports a
noImports = WithImports mempty

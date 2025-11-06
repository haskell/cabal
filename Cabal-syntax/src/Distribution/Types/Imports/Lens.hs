{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Types.Imports.Lens where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude

import qualified Distribution.Types.Imports as T
import qualified Distribution.Types.BuildInfo as T

import qualified Distribution.Types.BuildInfo.Lens as L

getImportNames :: Lens (T.WithImports a) (T.WithImports b) a b
getImportNames f (T.WithImports is x) = fmap (\y -> T.WithImports is y) (f x)
{-# INLINE getImportNames #-}

unImportNames :: Lens' (T.WithImports a) [T.ImportName]
unImportNames f (T.WithImports is x) = fmap (\is' -> T.WithImports is' x) (f is)
{-# INLINE unImportNames #-}


instance L.HasBuildInfo a => L.HasBuildInfo (T.WithImports a) where
  buildInfo f (T.WithImports is x) = T.WithImports is <$> L.buildInfo f x

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.ExtraSource
  ( ExtraSource (..)
  , ExtraSourceClass (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Path (Build, FileOrDir (..), Pkg, RelativePath, SymbolicPath, relativeSymbolicPath, unsafeCoerceSymbolicPath)

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as PP

data family ExtraSource pkg

data instance ExtraSource Pkg = ExtraSourcePkg (SymbolicPath Pkg File) [String]
  deriving (Generic, Show, Read, Eq, Ord, Data)

data instance ExtraSource Build = ExtraSourceBuild (RelativePath Build File) [String]
  deriving (Generic, Show, Read, Eq, Ord, Data)

class ExtraSourceClass e where
  extraSourceOpts :: e -> [String]
  extraSourceFile :: e -> SymbolicPath Pkg 'File

instance ExtraSourceClass (ExtraSource Pkg) where
  extraSourceOpts (ExtraSourcePkg _ opts) = opts
  extraSourceFile (ExtraSourcePkg f _) = f

instance ExtraSourceClass (ExtraSource Build) where
  extraSourceOpts (ExtraSourceBuild _ opts) = opts

  -- FIXME
  extraSourceFile (ExtraSourceBuild f _) = unsafeCoerceSymbolicPath (relativeSymbolicPath f)

instance Binary (ExtraSource Pkg)
instance Structured (ExtraSource Pkg)
instance NFData (ExtraSource Pkg) where rnf = genericRnf

instance Binary (ExtraSource Build)
instance Structured (ExtraSource Build)
instance NFData (ExtraSource Build) where rnf = genericRnf

instance Parsec (ExtraSource Pkg) where
  parsec = do
    path <- parsec <* P.spaces
    opts <- P.optional (parensLax (P.sepBy p P.spaces))
    return (ExtraSourcePkg path (fromMaybe mempty opts))
    where
      p :: P.CharParsing p => p String
      p = some $ P.satisfy (\c -> not (isSpace c) && (c /= ')'))

instance Parsec (ExtraSource Build) where
  parsec = do
    path <- parsec <* P.spaces
    opts <- P.optional (parensLax (P.sepBy p P.spaces))
    return (ExtraSourceBuild path (fromMaybe mempty opts))
    where
      p :: P.CharParsing p => p String
      p = some $ P.satisfy (\c -> not (isSpace c) && (c /= ')'))

instance Pretty (ExtraSource Pkg) where
  pretty (ExtraSourcePkg path opts) =
    pretty path <<>> PP.parens (PP.hsep (map PP.text opts))

instance Pretty (ExtraSource Build) where
  pretty (ExtraSourceBuild path opts) =
    pretty path <<>> PP.parens (PP.hsep (map PP.text opts))

parensLax :: P.CharParsing m => m a -> m a
parensLax p = P.between (P.char '(' *> P.spaces) (P.char ')' *> P.spaces) p

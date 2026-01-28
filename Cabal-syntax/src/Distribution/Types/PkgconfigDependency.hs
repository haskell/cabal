{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.PkgconfigDependency
  ( PkgconfigDependency (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.PkgconfigName
import Distribution.Types.PkgconfigVersionRange
import Distribution.Types.Annotation

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P

-- | Describes a dependency on a pkg-config library
--
-- @since 2.0.0.2
data PkgconfigDependency
  = PkgconfigDependency
      PkgconfigName
      PkgconfigVersionRange
  deriving (Generic, Read, Show, Eq, Ord, Data)

instance Binary PkgconfigDependency
instance Structured PkgconfigDependency
instance NFData PkgconfigDependency where rnf = genericRnf

instance Pretty PkgconfigDependency where
  pretty (PkgconfigDependency name PcAnyVersion) = pretty name
  pretty (PkgconfigDependency name ver) = pretty name <+> pretty ver

instance Markable PkgconfigDependency
instance ExactPretty PkgconfigDependency where
  exactPretty _ = pretty

-- TODO(leana8959): dropped spaces
instance ExactParsec PkgconfigDependency where exactParsec = (mempty,) <$> parsec
instance Parsec PkgconfigDependency where
  parsec = do
    name <- parsec
    P.spaces
    verRange <- parsec <|> pure anyPkgconfigVersion
    pure $ PkgconfigDependency name verRange

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Distribution.Types.PkgconfigDependency
  ( PkgconfigDependency(..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Version (VersionRange, anyVersion)

import Distribution.Types.PkgconfigName

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
import           Text.PrettyPrint           ((<+>))

-- | Describes a dependency on a pkg-config library
--
-- @since 2.0.0.2
data PkgconfigDependency = PkgconfigDependency
                           PkgconfigName
                           VersionRange
                         deriving (Generic, Read, Show, Eq, Typeable, Data)

instance Binary PkgconfigDependency
instance NFData PkgconfigDependency where rnf = genericRnf

instance Pretty PkgconfigDependency where
  pretty (PkgconfigDependency name ver) =
    pretty name <+> pretty ver

instance Parsec PkgconfigDependency where
    parsec = do
        name <- parsec
        P.spaces
        verRange <- parsec <|> pure anyVersion
        pure $ PkgconfigDependency name verRange

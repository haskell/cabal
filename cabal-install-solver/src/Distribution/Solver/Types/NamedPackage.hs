{-# LANGUAGE DeriveGeneric #-}

module Distribution.Solver.Types.NamedPackage
  ( NamedPackage (..)
  , NamedPackageConstraint
  ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageName (PackageName)
import Distribution.Solver.Types.PackageConstraint (PackageProperty)
import Distribution.Solver.Types.WithConstraintSource (WithConstraintSource)
import Distribution.Pretty (Pretty (pretty), commaSpaceSep)
import Text.PrettyPrint

-- | A package, identified by a name and properties.
data NamedPackage = NamedPackage PackageName [PackageProperty]
  deriving (Show, Eq, Ord, Generic)

instance Binary NamedPackage
instance Structured NamedPackage

instance Pretty NamedPackage where
  pretty (NamedPackage name properties) =
    pretty name <+> parens (commaSpaceSep properties)

type NamedPackageConstraint = WithConstraintSource NamedPackage

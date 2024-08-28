{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.PackageSpecifier
  ( PackageSpecifier (..)
  , pkgSpecifierTarget
  , pkgSpecifierConstraints
  , mkNamedPackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Package (Package (..), PackageIdentifier (..), packageName, packageVersion)
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (nullVersion, thisVersion)

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.PackageConstraint

-- | A fully or partially resolved reference to a package.
data PackageSpecifier pkg
  = -- | A partially specified reference to a package (either source or
    -- installed). It is specified by package name and optionally some
    -- required properties. Use a dependency resolver to pick a specific
    -- package satisfying these properties.
    NamedPackage PackageName [PackageProperty]
  | -- | A fully specified source package.
    SpecificSourcePackage pkg
  deriving (Eq, Show, Functor, Generic)

instance Binary pkg => Binary (PackageSpecifier pkg)
instance Structured pkg => Structured (PackageSpecifier pkg)

pkgSpecifierTarget :: Package pkg => PackageSpecifier pkg -> PackageName
pkgSpecifierTarget (NamedPackage name _) = name
pkgSpecifierTarget (SpecificSourcePackage pkg) = packageName pkg

pkgSpecifierConstraints
  :: Package pkg
  => PackageSpecifier pkg
  -> [LabeledPackageConstraint]
pkgSpecifierConstraints (NamedPackage name props) = map toLpc props
  where
    toLpc prop =
      LabeledPackageConstraint
        (PackageConstraint (scopeToplevel name) prop)
        ConstraintSourceUserTarget
pkgSpecifierConstraints (SpecificSourcePackage pkg) =
  [LabeledPackageConstraint pc ConstraintSourceUserTarget]
  where
    pc =
      PackageConstraint
        (ScopeTarget $ packageName pkg)
        (PackagePropertyVersion $ thisVersion (packageVersion pkg))

mkNamedPackage :: PackageIdentifier -> PackageSpecifier pkg
mkNamedPackage pkgId =
  NamedPackage
    (pkgName pkgId)
    ( if pkgVersion pkgId == nullVersion
        then []
        else [PackagePropertyVersion (thisVersion (pkgVersion pkgId))]
    )

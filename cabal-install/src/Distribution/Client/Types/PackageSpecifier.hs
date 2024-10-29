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

import Distribution.Client.Types.PackageLocation
import Distribution.Package (Package (..), PackageIdentifier (..), packageName, packageVersion)
import Distribution.Types.PackageName (PackageName)
import Distribution.Version (nullVersion, thisVersion)

import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.NamedPackage
  ( NamedPackage (..)
  , NamedPackageConstraint
  )
import Distribution.Solver.Types.PackageConstraint
import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.WithConstraintSource (WithConstraintSource (..))

-- | A fully or partially resolved reference to a package.
data PackageSpecifier pkg
  = -- | A partially specified reference to a package (either source or
    -- installed). It is specified by package name and optionally some
    -- required properties. Use a dependency resolver to pick a specific
    -- package satisfying these properties.
    Named NamedPackageConstraint
  | -- | A fully specified source package.
    SpecificSourcePackage pkg
  deriving (Eq, Show, Functor, Generic)

instance Binary pkg => Binary (PackageSpecifier pkg)
instance Structured pkg => Structured (PackageSpecifier pkg)

pkgSpecifierTarget :: Package pkg => PackageSpecifier pkg -> PackageName
pkgSpecifierTarget (Named (WithConstraintSource{constraintInner = NamedPackage name _})) = name
pkgSpecifierTarget (SpecificSourcePackage pkg) = packageName pkg

toConstraintSource :: UnresolvedSourcePackage -> ConstraintSource
toConstraintSource
  SourcePackage
    { srcpkgSource =
      WithConstraintSource
        { constraintSource = constraint
        }
    } = constraint

pkgSpecifierConstraints
  :: PackageSpecifier UnresolvedSourcePackage
  -> [LabeledPackageConstraint]
pkgSpecifierConstraints
  ( Named
      ( WithConstraintSource
          { constraintInner = NamedPackage name props
          , constraintSource = constraint
          }
        )
    ) =
    map toLpc props
    where
      toLpc prop =
        LabeledPackageConstraint
          (PackageConstraint (scopeToplevel name) prop)
          constraint
pkgSpecifierConstraints (SpecificSourcePackage pkg) =
  [LabeledPackageConstraint pc (toConstraintSource pkg)]
  where
    pc =
      PackageConstraint
        (ScopeTarget $ packageName pkg)
        (PackagePropertyVersion $ thisVersion (packageVersion pkg))

mkNamedPackage :: ConstraintSource -> PackageIdentifier -> PackageSpecifier pkg
mkNamedPackage constraint pkgId =
  Named
    ( WithConstraintSource
        { constraintInner =
            NamedPackage
              (pkgName pkgId)
              ( if pkgVersion pkgId == nullVersion
                  then []
                  else [PackagePropertyVersion (thisVersion (pkgVersion pkgId))]
              )
        , constraintSource = constraint
        }
    )

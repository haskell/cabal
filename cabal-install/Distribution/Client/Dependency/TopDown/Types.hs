-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.TopDown.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for the top-down dependency resolver.
-----------------------------------------------------------------------------
module Distribution.Client.Dependency.TopDown.Types where

import Distribution.Client.Types
         ( SourcePackage(..), InstalledPackage, OptionalStanza )

import Distribution.Package
         ( PackageIdentifier, Dependency
         , Package(packageId), PackageFixedDeps(depends) )
import Distribution.PackageDescription
         ( FlagAssignment )

-- ------------------------------------------------------------
-- * The various kinds of packages
-- ------------------------------------------------------------

type SelectablePackage
   = InstalledOrSource InstalledPackageEx UnconfiguredPackage

type SelectedPackage
   = InstalledOrSource InstalledPackageEx SemiConfiguredPackage

data InstalledOrSource installed source
   = InstalledOnly      installed
   | SourceOnly                   source
   | InstalledAndSource installed source
  deriving Eq

type TopologicalSortNumber = Int

data InstalledPackageEx
   = InstalledPackageEx
       InstalledPackage
       !TopologicalSortNumber
       [PackageIdentifier]    -- transitive closure of installed deps

data UnconfiguredPackage
   = UnconfiguredPackage
       SourcePackage
       !TopologicalSortNumber
       FlagAssignment
       [OptionalStanza]

data SemiConfiguredPackage
   = SemiConfiguredPackage
       SourcePackage     -- package info
       FlagAssignment    -- total flag assignment for the package
       [OptionalStanza]  -- enabled optional stanzas
       [Dependency]      -- dependencies we end up with when we apply
                         -- the flag assignment

instance Package InstalledPackageEx where
  packageId (InstalledPackageEx p _ _) = packageId p

instance PackageFixedDeps InstalledPackageEx where
  depends (InstalledPackageEx _ _ deps) = deps

instance Package UnconfiguredPackage where
  packageId (UnconfiguredPackage p _ _ _) = packageId p

instance Package SemiConfiguredPackage where
  packageId (SemiConfiguredPackage p _ _ _) = packageId p

instance (Package installed, Package source)
      => Package (InstalledOrSource installed source) where
  packageId (InstalledOnly      p  ) = packageId p
  packageId (SourceOnly         p  ) = packageId p
  packageId (InstalledAndSource p _) = packageId p


-- | We can have constraints on selecting just installed or just source
-- packages.
--
-- In particular, installed packages can only depend on other installed
-- packages while packages that are not yet installed but which we plan to
-- install can depend on installed or other not-yet-installed packages.
--
data InstalledConstraint = InstalledConstraint
                         | SourceConstraint
  deriving (Eq, Show)

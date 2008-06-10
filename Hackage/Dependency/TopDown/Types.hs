-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Dependency.TopDown.Types
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Types for the top-down dependency resolver.
-----------------------------------------------------------------------------
module Hackage.Dependency.TopDown.Types where

import Hackage.Types
         ( AvailablePackage(..) )

import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package
         ( PackageIdentifier, Dependency, Package(packageId) )
import Distribution.PackageDescription
         ( FlagAssignment )

-- ------------------------------------------------------------
-- * The various kinds of packages
-- ------------------------------------------------------------

type SelectablePackage
   = InstalledOrAvailable InstalledPackage UnconfiguredPackage

type SelectedPackage
   = InstalledOrAvailable InstalledPackage SemiConfiguredPackage

data InstalledOrAvailable installed available
   = InstalledOnly         installed
   | AvailableOnly                   available
   | InstalledAndAvailable installed available

type TopologicalSortNumber = Int

data InstalledPackage
   = InstalledPackage
       InstalledPackageInfo
       !TopologicalSortNumber
       [PackageIdentifier]

data UnconfiguredPackage
   = UnconfiguredPackage
       AvailablePackage
       !TopologicalSortNumber
       FlagAssignment

data SemiConfiguredPackage
   = SemiConfiguredPackage
       AvailablePackage  -- package info
       FlagAssignment    -- total flag assignment for the package
       [Dependency]      -- dependencies we end up with when we apply
                         -- the flag assignment

instance Package InstalledPackage where
  packageId (InstalledPackage p _ _) = packageId p

instance Package UnconfiguredPackage where
  packageId (UnconfiguredPackage p _ _) = packageId p

instance Package SemiConfiguredPackage where
  packageId (SemiConfiguredPackage p _ _) = packageId p

instance (Package installed, Package available)
      => Package (InstalledOrAvailable installed available) where
  packageId (InstalledOnly         p  ) = packageId p
  packageId (AvailableOnly         p  ) = packageId p
  packageId (InstalledAndAvailable p _) = packageId p

-- ------------------------------------------------------------
-- * Tagged Dependency type
-- ------------------------------------------------------------

-- | Installed packages can only depend on other installed packages while
-- packages that are not yet installed but which we plan to install can depend
-- on installed or other not-yet-installed packages.
--
-- This makes life more complex as we have to remember these constraints.
--
data TaggedDependency = TaggedDependency InstalledConstraint Dependency
data InstalledConstraint = InstalledConstraint | NoInstalledConstraint
  deriving Eq

untagDependency :: TaggedDependency -> Dependency
untagDependency (TaggedDependency _ dep) = dep

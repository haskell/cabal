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
{-# LANGUAGE CPP #-}
module Distribution.Client.Dependency.TopDown.Types where

import Distribution.Client.Types
         ( SourcePackage(..), ConfiguredPackage(..)
         , OptionalStanza, ConfiguredId(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import qualified Distribution.Client.ComponentDeps as CD

import Distribution.Package
         ( PackageId, PackageIdentifier, Dependency
         , Package(packageId) )
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

data FinalSelectedPackage
   = SelectedInstalled InstalledPackage
   | SelectedSource    ConfiguredPackage

type TopologicalSortNumber = Int

-- | InstalledPackage caches its dependencies as source package IDs.
data InstalledPackage
   = InstalledPackage
       InstalledPackageInfo
       [PackageId]

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

instance Package InstalledPackage where
  packageId (InstalledPackage pkg _) = packageId pkg

instance Package InstalledPackageEx where
  packageId (InstalledPackageEx p _ _) = packageId p

instance Package UnconfiguredPackage where
  packageId (UnconfiguredPackage p _ _ _) = packageId p

instance Package SemiConfiguredPackage where
  packageId (SemiConfiguredPackage p _ _ _) = packageId p

instance (Package installed, Package source)
      => Package (InstalledOrSource installed source) where
  packageId (InstalledOnly      p  ) = packageId p
  packageId (SourceOnly         p  ) = packageId p
  packageId (InstalledAndSource p _) = packageId p

instance Package FinalSelectedPackage where
  packageId (SelectedInstalled pkg) = packageId pkg
  packageId (SelectedSource    pkg) = packageId pkg


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

-- | Package dependencies
--
-- The top-down solver uses its down type class for package dependencies,
-- because it wants to know these dependencies as PackageIds, rather than as
-- ComponentIds (so it cannot use PackageFixedDeps).
--
-- Ideally we would switch the top-down solver over to use ComponentIds
-- throughout; that means getting rid of this type class, and changing over the
-- package index type to use Cabal's rather than cabal-install's. That will
-- avoid the need for the local definitions of dependencyGraph and
-- reverseTopologicalOrder in the top-down solver.
--
-- Note that the top-down solver does not (and probably will never) make a
-- distinction between the various kinds of dependencies, so we return a flat
-- list here. If we get rid of this type class then any use of `sourceDeps`
-- should be replaced by @fold . depends@.
class Package a => PackageSourceDeps a where
  sourceDeps :: a -> [PackageIdentifier]

instance PackageSourceDeps InstalledPackageEx where
  sourceDeps (InstalledPackageEx _ _ deps) = deps

instance PackageSourceDeps ConfiguredPackage where
  sourceDeps (ConfiguredPackage _ _ _ deps) = map confSrcId $ CD.nonSetupDeps deps

instance PackageSourceDeps InstalledPackage where
  sourceDeps (InstalledPackage _ deps) = deps

instance PackageSourceDeps FinalSelectedPackage where
  sourceDeps (SelectedInstalled pkg) = sourceDeps pkg
  sourceDeps (SelectedSource    pkg) = sourceDeps pkg


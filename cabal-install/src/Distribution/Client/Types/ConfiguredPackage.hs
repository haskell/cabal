{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Client.Types.ConfiguredPackage
  ( ConfiguredPackage (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Compat.Graph (IsNode (..))
import Distribution.Package (HasMungedPackageId (..), HasUnitId (..), Package (..), PackageInstalled (..), UnitId, newSimpleUnitId)
import Distribution.Simple.Utils (ordNub)
import Distribution.Types.ComponentName
import Distribution.Types.Flag (FlagAssignment)
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.MungedPackageId (computeCompatPackageId)

import Distribution.Client.Types.ConfiguredId
import Distribution.Solver.Types.OptionalStanza (OptionalStanzaSet)
import Distribution.Solver.Types.PackageFixedDeps
import Distribution.Solver.Types.SourcePackage (SourcePackage)

import qualified Distribution.Solver.Types.ComponentDeps as CD

-- | A 'ConfiguredPackage' is a not-yet-installed package along with the
-- total configuration information. The configuration information is total in
-- the sense that it provides all the configuration information and so the
-- final configure process will be independent of the environment.
--
-- 'ConfiguredPackage' is assumed to not support Backpack.  Only the
-- @v2-build@ codepath supports Backpack.
data ConfiguredPackage loc = ConfiguredPackage
  { confPkgId :: InstalledPackageId
  , confPkgSource :: SourcePackage loc
  -- ^ package info, including repo
  , confPkgFlags :: FlagAssignment
  -- ^ complete flag assignment for the package
  , confPkgStanzas :: OptionalStanzaSet
  -- ^ list of enabled optional stanzas for the package
  , confPkgDeps :: CD.ComponentDeps [ConfiguredId]
  -- ^ set of exact dependencies (installed or source).
  --
  -- These must be consistent with the 'buildDepends'
  -- in the 'PackageDescription' that you'd get by
  -- applying the flag assignment and optional stanzas.
  }
  deriving (Eq, Show, Generic)

-- | 'HasConfiguredId' indicates data types which have a 'ConfiguredId'.
-- This type class is mostly used to conveniently finesse between
-- 'ElaboratedPackage' and 'ElaboratedComponent'.
instance HasConfiguredId (ConfiguredPackage loc) where
  configuredId pkg = ConfiguredId (packageId pkg) (Just (CLibName LMainLibName)) (confPkgId pkg)

-- 'ConfiguredPackage' is the legacy codepath, we are guaranteed
-- to never have a nontrivial 'UnitId'
instance PackageFixedDeps (ConfiguredPackage loc) where
  depends = fmap (map (newSimpleUnitId . confInstId)) . confPkgDeps

instance IsNode (ConfiguredPackage loc) where
  type Key (ConfiguredPackage loc) = UnitId
  nodeKey = newSimpleUnitId . confPkgId

  -- TODO: if we update ConfiguredPackage to support order-only
  -- dependencies, need to include those here.
  -- NB: have to deduplicate, otherwise the planner gets confused
  nodeNeighbors = ordNub . CD.flatDeps . depends

instance Binary loc => Binary (ConfiguredPackage loc)

instance Package (ConfiguredPackage loc) where
  packageId cpkg = packageId (confPkgSource cpkg)

instance HasMungedPackageId (ConfiguredPackage loc) where
  mungedId cpkg = computeCompatPackageId (packageId cpkg) LMainLibName

-- Never has nontrivial UnitId
instance HasUnitId (ConfiguredPackage loc) where
  installedUnitId = newSimpleUnitId . confPkgId

instance PackageInstalled (ConfiguredPackage loc) where
  installedDepends = CD.flatDeps . depends

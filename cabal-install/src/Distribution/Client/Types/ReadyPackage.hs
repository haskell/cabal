{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.Client.Types.ReadyPackage
  ( GenericReadyPackage (..)
  , ReadyPackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Compat.Graph (IsNode (..))
import Distribution.Package (HasMungedPackageId, HasUnitId, Package, PackageInstalled)

import Distribution.Client.Types.ConfiguredPackage (ConfiguredPackage)
import Distribution.Client.Types.PackageLocation (UnresolvedPkgLoc)
import Distribution.Solver.Types.PackageFixedDeps

-- | Like 'ConfiguredPackage', but with all dependencies guaranteed to be
-- installed already, hence itself ready to be installed.
newtype GenericReadyPackage srcpkg = ReadyPackage srcpkg -- see 'ConfiguredPackage'.
  deriving
    ( Eq
    , Show
    , Generic
    , Package
    , PackageFixedDeps
    , HasMungedPackageId
    , HasUnitId
    , PackageInstalled
    , Binary
    )

-- Can't newtype derive this
instance IsNode key srcpkg => IsNode key (GenericReadyPackage srcpkg) where
  nodeKey (ReadyPackage spkg) = nodeKey spkg
  nodeNeighbors (ReadyPackage spkg) = nodeNeighbors spkg

type ReadyPackage = GenericReadyPackage (ConfiguredPackage UnresolvedPkgLoc)

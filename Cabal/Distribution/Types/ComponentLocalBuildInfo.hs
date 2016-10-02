{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.ComponentLocalBuildInfo (
  ComponentLocalBuildInfo(..),
  componentComponentId,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Compat.Graph
import Distribution.Types.ComponentName

import Distribution.PackageDescription
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package

-- | The first five fields are common across all algebraic variants.
data ComponentLocalBuildInfo
  = LibComponentLocalBuildInfo {
    -- | It would be very convenient to store the literal Library here,
    -- but if we do that, it will get serialized (via the Binary)
    -- instance twice.  So instead we just provide the ComponentName,
    -- which can be used to find the Component in the
    -- PackageDescription.  NB: eventually, this will NOT uniquely
    -- identify the ComponentLocalBuildInfo.
    componentLocalName :: ComponentName,
    -- | The computed 'UnitId' which uniquely identifies this
    -- component.
    componentUnitId :: UnitId,
    -- | Resolved internal and external package dependencies for this component.
    -- The 'BuildInfo' specifies a set of build dependencies that must be
    -- satisfied in terms of version ranges. This field fixes those dependencies
    -- to the specific versions available on this machine for this compiler.
    componentPackageDeps :: [(UnitId, PackageId)],
    -- | The set of packages that are brought into scope during
    -- compilation, including a 'ModuleRenaming' which may used
    -- to hide or rename modules.  This is what gets translated into
    -- @-package-id@ arguments.  This is a modernized version of
    -- 'componentPackageDeps', which is kept around for BC purposes.
    componentIncludes :: [(UnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    -- | The internal dependencies which induce a graph on the
    -- 'ComponentLocalBuildInfo' of this package.  This does NOT
    -- coincide with 'componentPackageDeps' because it ALSO records
    -- 'build-tool' dependencies on executables.  Maybe one day
    -- @cabal-install@ will also handle these correctly too!
    componentInternalDeps :: [UnitId],
    -- | Compatibility "package key" that we pass to older versions of GHC.
    componentCompatPackageKey :: String,
    -- | Compatability "package name" that we register this component as.
    componentCompatPackageName :: PackageName,
    -- | A list of exposed modules (either defined in this component,
    -- or reexported from another component.)
    componentExposedModules :: [Installed.ExposedModule],
    -- | Convenience field, specifying whether or not this is the
    -- "public library" that has the same name as the package.
    componentIsPublic :: Bool
  }
  | ExeComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, PackageId)],
    componentIncludes :: [(UnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]
  }
  | TestComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, PackageId)],
    componentIncludes :: [(UnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]

  }
  | BenchComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, PackageId)],
    componentIncludes :: [(UnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]
  }
  deriving (Generic, Read, Show)

instance Binary ComponentLocalBuildInfo

instance IsNode ComponentLocalBuildInfo where
    type Key ComponentLocalBuildInfo = UnitId
    nodeKey = componentUnitId
    nodeNeighbors = componentInternalDeps

componentComponentId :: ComponentLocalBuildInfo -> ComponentId
componentComponentId clbi = unitIdComponentId (componentUnitId clbi)

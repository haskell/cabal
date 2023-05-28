-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.PreExistingComponent
  ( PreExistingComponent (..)
  , PromisedComponent (..)
  , ipiToPreExistingComponent
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.Backpack.ModuleShape
import Distribution.Package
import Distribution.Types.ComponentName
import Distribution.Types.MungedPackageId

import qualified Data.Map as Map
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Types.AnnotatedId

-- | A /promised/ component.
--
-- These components are promised to @configure@ but are not yet built.
--
-- In other words this is 'PreExistingComponent' which doesn't yet exist.
data PromisedComponent = PromisedComponent
  { pr_pkgname :: PackageName
  , pr_cid :: AnnotatedId ComponentId
  }

instance Package PromisedComponent where
  packageId = packageId . pr_cid

-- | Stripped down version of 'LinkedComponent' for things
-- we don't need to know how to build.
data PreExistingComponent = PreExistingComponent
  { pc_pkgname :: PackageName
  -- ^ The actual name of the package. This may DISAGREE with 'pc_pkgid'
  -- for internal dependencies: e.g., an internal component @lib@ may be
  -- munged to @z-pkg-z-lib@, but we still want to use it when we see
  -- @lib@ in @build-depends@
  , pc_compname :: ComponentName
  -- ^ The actual name of the component.
  , pc_munged_id :: MungedPackageId
  , pc_uid :: UnitId
  , pc_cid :: ComponentId
  , pc_open_uid :: OpenUnitId
  , pc_shape :: ModuleShape
  }

-- | Convert an 'InstalledPackageInfo' into a 'PreExistingComponent',
-- which was brought into scope under the 'PackageName' (important for
-- a package qualified reference.)
ipiToPreExistingComponent :: InstalledPackageInfo -> PreExistingComponent
ipiToPreExistingComponent ipi =
  PreExistingComponent
    { pc_pkgname = packageName ipi
    , pc_compname = CLibName $ Installed.sourceLibName ipi
    , pc_munged_id = mungedId ipi
    , pc_uid = Installed.installedUnitId ipi
    , pc_cid = Installed.installedComponentId ipi
    , pc_open_uid =
        IndefFullUnitId
          (Installed.installedComponentId ipi)
          (Map.fromList (Installed.instantiatedWith ipi))
    , pc_shape = shapeInstalledPackage ipi
    }

instance HasMungedPackageId PreExistingComponent where
  mungedId = pc_munged_id

instance Package PreExistingComponent where
  packageId pec = PackageIdentifier (pc_pkgname pec) v
    where
      MungedPackageId _ v = pc_munged_id pec

instance HasUnitId PreExistingComponent where
  installedUnitId = pc_uid

-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.PreExistingComponent (
    PreExistingComponent(..),
    pc_cid,
    ipiToPreExistingComponent,
) where

import Prelude ()

import Distribution.Backpack.ModuleShape
import Distribution.Backpack

import qualified Data.Map as Map
import Distribution.Package
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.InstalledPackageInfo (InstalledPackageInfo)

-- | Stripped down version of 'LinkedComponent' for things
-- we don't need to know how to build.
data PreExistingComponent
    = PreExistingComponent {
        -- | The 'PackageName' that, when we see it in 'PackageDescription',
        -- we should map this to.  This may DISAGREE with 'pc_pkgid' for
        -- internal dependencies: e.g., an internal component @lib@
        -- may be munged to @z-pkg-z-lib@, but we still want to use
        -- it when we see @lib@ in @build-depends@
        pc_pkgname :: PackageName,
        pc_pkgid :: PackageId,
        pc_uid   :: UnitId,
        pc_indef_uid :: IndefUnitId,
        pc_shape :: ModuleShape
    }

-- | The 'ComponentId' of a 'PreExistingComponent'.
pc_cid :: PreExistingComponent -> ComponentId
pc_cid pc = unitIdComponentId (pc_uid pc)

-- | Convert an 'InstalledPackageInfo' into a 'PreExistingComponent',
-- which was brought into scope under the 'PackageName' (important for
-- a package qualified reference.)
ipiToPreExistingComponent :: (PackageName, InstalledPackageInfo) -> PreExistingComponent
ipiToPreExistingComponent (pn, ipi) =
    PreExistingComponent {
        pc_pkgname = pn,
        pc_pkgid = Installed.sourcePackageId ipi,
        pc_uid   = Installed.installedUnitId ipi,
        pc_indef_uid =
            IndefFullUnitId (Installed.installedComponentId ipi)
                            (Map.fromList (Installed.instantiatedWith ipi)),
        pc_shape = shapeInstalledPackage ipi
    }


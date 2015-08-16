-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Dependency.Policies
-- Copyright   :  (c) David Himmelstrup 2005,
--                    Bjorn Bringert 2007
--                    Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Dependency resolution policies
-----------------------------------------------------------------------------

module Distribution.Client.Dependency.Policies (
    -- ** Standard policy
    standardInstallPolicy,
    PackageSpecifier(..),

    -- ** Sandbox policy
    applySandboxInstallPolicy,
  ) where

import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Package
import Distribution.Version
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
         ( LabeledPackageConstraint(..), ConstraintSource(..) )
import qualified Distribution.Simple.PackageIndex as InstalledPackageIndex
import Distribution.Client.Targets
import Distribution.Client.Sandbox.Types
         ( SandboxPackageInfo(..) )
import Distribution.Client.Types
         ( SourcePackageDb(SourcePackageDb), SourcePackage(..) )

import qualified Data.Map as Map

standardInstallPolicy :: InstalledPackageIndex
                      -> SourcePackageDb
                      -> [PackageSpecifier SourcePackage]
                      -> DepResolverParams
standardInstallPolicy
    installedPkgIndex (SourcePackageDb sourcePkgIndex sourcePkgPrefs)
    pkgSpecifiers

  = addPreferences
      [ PackageVersionPreference name ver
      | (name, ver) <- Map.toList sourcePkgPrefs ]

  . addConstraints
      (concatMap pkgSpecifierConstraints pkgSpecifiers)

  . addTargets
      (map pkgSpecifierTarget pkgSpecifiers)

  . hideInstalledPackagesSpecificBySourcePackageId
      [ packageId pkg | SpecificSourcePackage pkg <- pkgSpecifiers ]

  . addSourcePackages
      [ pkg  | SpecificSourcePackage pkg <- pkgSpecifiers ]

  $ basicDepResolverParams
      installedPkgIndex sourcePkgIndex

applySandboxInstallPolicy :: SandboxPackageInfo
                             -> DepResolverParams
                             -> DepResolverParams
applySandboxInstallPolicy
  (SandboxPackageInfo modifiedDeps otherDeps allSandboxPkgs _allDeps)
  params

  = addPreferences [ PackageInstalledPreference n PreferInstalled
                   | n <- installedNotModified ]

  . addTargets installedNotModified

  . addPreferences
      [ PackageVersionPreference (packageName pkg)
        (thisVersion (packageVersion pkg)) | pkg <- otherDeps ]

  . addConstraints
      [ let pc = PackageConstraintVersion (packageName pkg)
                 (thisVersion (packageVersion pkg))
        in LabeledPackageConstraint pc ConstraintSourceModifiedAddSourceDep
      | pkg <- modifiedDeps ]

  . addTargets [ packageName pkg | pkg <- modifiedDeps ]

  . hideInstalledPackagesSpecificBySourcePackageId
      [ packageId pkg | pkg <- modifiedDeps ]

  -- We don't need to add source packages for add-source deps to the
  -- 'installedPkgIndex' since 'getSourcePackages' did that for us.

  $ params

  where
    installedPkgIds =
      map fst . InstalledPackageIndex.allPackagesBySourcePackageId
      $ allSandboxPkgs
    modifiedPkgIds       = map packageId modifiedDeps
    installedNotModified = [ packageName pkg | pkg <- installedPkgIds,
                             pkg `notElem` modifiedPkgIds ]

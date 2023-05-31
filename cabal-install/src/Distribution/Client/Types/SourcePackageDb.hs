{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.SourcePackageDb
  ( SourcePackageDb (..)
  , lookupDependency
  , lookupPackageName
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Package (packageVersion)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.VersionRange (VersionRange, withinRange)

import Distribution.Client.Types.PackageLocation (UnresolvedSourcePackage)
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex

import qualified Data.Map as Map

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
data SourcePackageDb = SourcePackageDb
  { packageIndex :: PackageIndex UnresolvedSourcePackage
  , packagePreferences :: Map PackageName VersionRange
  }
  deriving (Eq, Generic)

instance Binary SourcePackageDb

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
-- Additionally, `preferred-versions` (such as version deprecation) are
-- honoured in this lookup, which is the only difference to
-- 'PackageIndex.lookupDependency'
lookupDependency :: SourcePackageDb -> PackageName -> VersionRange -> [UnresolvedSourcePackage]
lookupDependency sourceDb pname version =
  filterPreferredVersions pref $ PackageIndex.lookupDependency (packageIndex sourceDb) pname version
  where
    pref = Map.lookup pname (packagePreferences sourceDb)

-- | Does a case-sensitive search by package name.
--
-- Additionally, `preferred-versions` (such as version deprecation) are
-- honoured in this lookup, which is the only difference to
-- 'PackageIndex.lookupPackageName'
lookupPackageName :: SourcePackageDb -> PackageName -> [UnresolvedSourcePackage]
lookupPackageName sourceDb pname =
  filterPreferredVersions pref $ PackageIndex.lookupPackageName (packageIndex sourceDb) pname
  where
    pref = Map.lookup pname (packagePreferences sourceDb)

-- | @filterPreferredVersions 'range' 'versions'@.
-- If a 'range' is given, only keep versions that satisfy the range.
-- If 'range' is 'Nothing', all versions are kept.
--
-- The 'range' is expected to be obtained from the 'SourcePackageDb.packagePreferences'.
filterPreferredVersions :: Maybe VersionRange -> [UnresolvedSourcePackage] -> [UnresolvedSourcePackage]
filterPreferredVersions Nothing versions = versions
filterPreferredVersions (Just range) versions = filter ((`withinRange` range) . packageVersion) versions

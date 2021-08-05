{-# LANGUAGE DeriveGeneric #-}
module Distribution.Client.Types.SourcePackageDb (
    SourcePackageDb (..),
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageName  (PackageName)
import Distribution.Types.VersionRange (VersionRange)

import Distribution.Client.Types.PackageLocation (UnresolvedSourcePackage)
import Distribution.Solver.Types.PackageIndex    (PackageIndex)

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
--
data SourcePackageDb = SourcePackageDb
    { packageIndex       :: PackageIndex UnresolvedSourcePackage
    , packagePreferences :: Map PackageName VersionRange
    }
  deriving (Eq, Generic)

instance Binary SourcePackageDb

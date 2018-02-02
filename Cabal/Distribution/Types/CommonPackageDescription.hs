{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Types.CommonPackageDescription (
    CommonPackageDescription(..),
    emptyCommonPackageDescription,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.SetupBuildInfo
import Distribution.Types.SourceRepo

import Distribution.Compiler
import Distribution.Package
import Distribution.Version

-- -----------------------------------------------------------------------------
-- The CommonPackageDescription type

-- | This data type holds the fields common to both 'PackageDescription' and
-- 'GenericCommonPackageDescription'.
data CommonPackageDescription =
  CommonPackageDescription
  {
    -- the following are required by all packages:

    package        :: PackageIdentifier
  , licenseFiles   :: [FilePath]
  , copyright      :: String
  , maintainer     :: String
  , author         :: String
  , stability      :: String
  , testedWith     :: [(CompilerFlavor,VersionRange)]
  , homepage       :: String
  , pkgUrl         :: String
  , bugReports     :: String
  , sourceRepos    :: [SourceRepo]
  , -- | A one-line summary of this package
    synopsis       :: String
  , -- | A more verbose description of this package
    description    :: String
  , category       :: String
  , -- | Custom fields starting with x-, stored in a simple assoc-list.
    customFieldsPD :: [(String,String)]

  , -- | TODO(@Ericson2314): this doesn't really belong here. There should be
    -- some limited cond tree or similar instead in 'GenericPackageDescription',
    -- and this in 'PackageDescription'.
    setupBuildInfo :: Maybe SetupBuildInfo
    -- files
  , dataFiles      :: [FilePath]
  , dataDir        :: FilePath
  , extraSrcFiles  :: [FilePath]
  , extraTmpFiles  :: [FilePath]
  , extraDocFiles  :: [FilePath]
  }
  deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary CommonPackageDescription

instance NFData CommonPackageDescription where rnf = genericRnf

instance Package CommonPackageDescription where
  packageId = package

emptyCommonPackageDescription :: CommonPackageDescription
emptyCommonPackageDescription =
  CommonPackageDescription
  { package        = PackageIdentifier (mkPackageName "") nullVersion
  , licenseFiles   = []
  , copyright      = ""
  , maintainer     = ""
  , author         = ""
  , stability      = ""
  , testedWith     = []
  , homepage       = ""
  , pkgUrl         = ""
  , bugReports     = ""
  , sourceRepos    = []
  , synopsis       = ""
  , description    = ""
  , category       = ""
  , customFieldsPD = []
  , setupBuildInfo = Nothing
  , dataFiles      = []
  , dataDir        = ""
  , extraSrcFiles  = []
  , extraTmpFiles  = []
  , extraDocFiles  = []
  }


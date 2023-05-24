{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.InstalledPackageInfo
  ( InstalledPackageInfo (..)
  , emptyInstalledPackageInfo
  , mungedPackageId
  , mungedPackageName
  , AbiDependency (..)
  , ExposedModule (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.Compat.Graph (IsNode (..))
import Distribution.License
import Distribution.ModuleName
import Distribution.Package hiding (installedUnitId)
import Distribution.Types.AbiDependency
import Distribution.Types.ExposedModule
import Distribution.Types.LibraryName
import Distribution.Types.LibraryVisibility
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Utils.ShortText (ShortText)
import Distribution.Version (nullVersion)

import qualified Distribution.Package as Package
import qualified Distribution.SPDX as SPDX

-- -----------------------------------------------------------------------------
-- The InstalledPackageInfo type

-- For BC reasons, we continue to name this record an InstalledPackageInfo;
-- but it would more accurately be called an InstalledUnitInfo with Backpack
data InstalledPackageInfo = InstalledPackageInfo
  { -- these parts (sourcePackageId, installedUnitId) are
    -- exactly the same as PackageDescription
    sourcePackageId :: PackageId
  , sourceLibName :: LibraryName
  , installedComponentId_ :: ComponentId
  , libVisibility :: LibraryVisibility
  , installedUnitId :: UnitId
  , -- INVARIANT: if this package is definite, OpenModule's
    -- OpenUnitId directly records UnitId.  If it is
    -- indefinite, OpenModule is always an OpenModuleVar
    -- with the same ModuleName as the key.
    instantiatedWith :: [(ModuleName, OpenModule)]
  , compatPackageKey :: String
  , license :: Either SPDX.License License
  , copyright :: !ShortText
  , maintainer :: !ShortText
  , author :: !ShortText
  , stability :: !ShortText
  , homepage :: !ShortText
  , pkgUrl :: !ShortText
  , synopsis :: !ShortText
  , description :: !ShortText
  , category :: !ShortText
  , -- these parts are required by an installed package only:
    abiHash :: AbiHash
  , indefinite :: Bool
  , exposed :: Bool
  , -- INVARIANT: if the package is definite, OpenModule's
    -- OpenUnitId directly records UnitId.
    exposedModules :: [ExposedModule]
  , hiddenModules :: [ModuleName]
  , trusted :: Bool
  , importDirs :: [FilePath]
  , libraryDirs :: [FilePath]
  , libraryDirsStatic :: [FilePath]
  , libraryDynDirs :: [FilePath]
  -- ^ overrides 'libraryDirs'
  , dataDir :: FilePath
  , hsLibraries :: [String]
  , extraLibraries :: [String]
  , extraLibrariesStatic :: [String]
  , extraGHCiLibraries :: [String] -- overrides extraLibraries for GHCi
  , includeDirs :: [FilePath]
  , includes :: [String]
  , -- INVARIANT: if the package is definite, UnitId is NOT
    -- a ComponentId of an indefinite package
    depends :: [UnitId]
  , abiDepends :: [AbiDependency]
  , ccOptions :: [String]
  , cxxOptions :: [String]
  , ldOptions :: [String]
  , frameworkDirs :: [FilePath]
  , frameworks :: [String]
  , haddockInterfaces :: [FilePath]
  , haddockHTMLs :: [FilePath]
  , pkgRoot :: Maybe FilePath
  }
  deriving (Eq, Generic, Typeable, Read, Show)

instance Binary InstalledPackageInfo
instance Structured InstalledPackageInfo

instance NFData InstalledPackageInfo where rnf = genericRnf

instance Package.HasMungedPackageId InstalledPackageInfo where
  mungedId = mungedPackageId

instance Package.Package InstalledPackageInfo where
  packageId = sourcePackageId

instance Package.HasUnitId InstalledPackageInfo where
  installedUnitId = installedUnitId

instance Package.PackageInstalled InstalledPackageInfo where
  installedDepends = depends

instance IsNode InstalledPackageInfo where
  type Key InstalledPackageInfo = UnitId
  nodeKey = installedUnitId
  nodeNeighbors = depends

mungedPackageId :: InstalledPackageInfo -> MungedPackageId
mungedPackageId ipi =
  MungedPackageId (mungedPackageName ipi) (packageVersion ipi)

-- | Returns the munged package name, which we write into @name@ for
-- compatibility with old versions of GHC.
mungedPackageName :: InstalledPackageInfo -> MungedPackageName
mungedPackageName ipi = MungedPackageName (packageName ipi) (sourceLibName ipi)

emptyInstalledPackageInfo :: InstalledPackageInfo
emptyInstalledPackageInfo =
  InstalledPackageInfo
    { sourcePackageId = PackageIdentifier (mkPackageName "") nullVersion
    , sourceLibName = LMainLibName
    , installedComponentId_ = mkComponentId ""
    , installedUnitId = mkUnitId ""
    , instantiatedWith = []
    , compatPackageKey = ""
    , license = Left SPDX.NONE
    , copyright = ""
    , maintainer = ""
    , author = ""
    , stability = ""
    , homepage = ""
    , pkgUrl = ""
    , synopsis = ""
    , description = ""
    , category = ""
    , abiHash = mkAbiHash ""
    , indefinite = False
    , exposed = False
    , exposedModules = []
    , hiddenModules = []
    , trusted = False
    , importDirs = []
    , libraryDirs = []
    , libraryDirsStatic = []
    , libraryDynDirs = []
    , dataDir = ""
    , hsLibraries = []
    , extraLibraries = []
    , extraLibrariesStatic = []
    , extraGHCiLibraries = []
    , includeDirs = []
    , includes = []
    , depends = []
    , abiDepends = []
    , ccOptions = []
    , cxxOptions = []
    , ldOptions = []
    , frameworkDirs = []
    , frameworks = []
    , haddockInterfaces = []
    , haddockHTMLs = []
    , pkgRoot = Nothing
    , libVisibility = LibraryVisibilityPrivate
    }

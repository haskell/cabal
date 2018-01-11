{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Types.InstalledPackageInfo (
    InstalledPackageInfo (..),
    mungedPackageId,
    mungedPackageName,
    AbiDependency (..),
    ExposedModule (..),
    ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Backpack
import Distribution.License
import Distribution.ModuleName
import Distribution.Package hiding  (installedUnitId)
import Distribution.ParseUtils                (parseModuleNameQ)
import Distribution.Text
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName
import Distribution.Types.UnqualComponentName
import Distribution.Compat.Graph (IsNode (..))

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint          as Disp
import qualified Distribution.Package as Package

-- -----------------------------------------------------------------------------
-- The InstalledPackageInfo type

-- For BC reasons, we continue to name this record an InstalledPackageInfo;
-- but it would more accurately be called an InstalledUnitInfo with Backpack
data InstalledPackageInfo
   = InstalledPackageInfo {
        -- these parts are exactly the same as PackageDescription
        sourcePackageId   :: PackageId,
        installedUnitId   :: UnitId,
        installedComponentId_ :: ComponentId,
        -- INVARIANT: if this package is definite, OpenModule's
        -- OpenUnitId directly records UnitId.  If it is
        -- indefinite, OpenModule is always an OpenModuleVar
        -- with the same ModuleName as the key.
        instantiatedWith  :: [(ModuleName, OpenModule)],
        sourceLibName     :: Maybe UnqualComponentName,
        compatPackageKey  :: String,
        license           :: License,
        copyright         :: String,
        maintainer        :: String,
        author            :: String,
        stability         :: String,
        homepage          :: String,
        pkgUrl            :: String,
        synopsis          :: String,
        description       :: String,
        category          :: String,
        -- these parts are required by an installed package only:
        abiHash           :: AbiHash,
        indefinite        :: Bool,
        exposed           :: Bool,
        -- INVARIANT: if the package is definite, OpenModule's
        -- OpenUnitId directly records UnitId.
        exposedModules    :: [ExposedModule],
        hiddenModules     :: [ModuleName],
        trusted           :: Bool,
        importDirs        :: [FilePath],
        libraryDirs       :: [FilePath],
        libraryDynDirs    :: [FilePath],  -- ^ overrides 'libraryDirs'
        dataDir           :: FilePath,
        hsLibraries       :: [String],
        extraLibraries    :: [String],
        extraGHCiLibraries:: [String],    -- overrides extraLibraries for GHCi
        includeDirs       :: [FilePath],
        includes          :: [String],
        -- INVARIANT: if the package is definite, UnitId is NOT
        -- a ComponentId of an indefinite package
        depends           :: [UnitId],
        abiDepends        :: [AbiDependency],
        ccOptions         :: [String],
        ldOptions         :: [String],
        frameworkDirs     :: [FilePath],
        frameworks        :: [String],
        haddockInterfaces :: [FilePath],
        haddockHTMLs      :: [FilePath],
        pkgRoot           :: Maybe FilePath
    }
    deriving (Eq, Generic, Typeable, Read, Show)

instance Binary InstalledPackageInfo

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
    nodeKey       = installedUnitId
    nodeNeighbors = depends

mungedPackageId :: InstalledPackageInfo -> MungedPackageId
mungedPackageId ipi =
    MungedPackageId (mungedPackageName ipi) (packageVersion ipi)

-- | Returns the munged package name, which we write into @name@ for
-- compatibility with old versions of GHC.
mungedPackageName :: InstalledPackageInfo -> MungedPackageName
mungedPackageName ipi =
    computeCompatPackageName
        (packageName ipi)
        (sourceLibName ipi)

-- -----------------------------------------------------------------------------
-- ABI dependency

-- | An ABI dependency is a dependency on a library which also
-- records the ABI hash ('abiHash') of the library it depends
-- on.
--
-- The primary utility of this is to enable an extra sanity when
-- GHC loads libraries: it can check if the dependency has a matching
-- ABI and if not, refuse to load this library.  This information
-- is critical if we are shadowing libraries; differences in the
-- ABI hash let us know what packages get shadowed by the new version
-- of a package.
data AbiDependency = AbiDependency {
        depUnitId  :: UnitId,
        depAbiHash :: AbiHash
    }
  deriving (Eq, Generic, Read, Show)

instance Text AbiDependency where
    disp (AbiDependency uid abi) =
        disp uid <<>> Disp.char '=' <<>> disp abi
    parse = do
        uid <- parse
        _ <- Parse.char '='
        abi <- parse
        return (AbiDependency uid abi)

instance Binary AbiDependency

-- -----------------------------------------------------------------------------
-- Exposed modules

data ExposedModule
   = ExposedModule {
       exposedName      :: ModuleName,
       exposedReexport  :: Maybe OpenModule
     }
  deriving (Eq, Generic, Read, Show)

instance Text ExposedModule where
    disp (ExposedModule m reexport) =
        Disp.hsep [ disp m
                  , case reexport of
                     Just m' -> Disp.hsep [Disp.text "from", disp m']
                     Nothing -> Disp.empty
                  ]
    parse = do
        m <- parseModuleNameQ
        Parse.skipSpaces
        reexport <- Parse.option Nothing $ do
            _ <- Parse.string "from"
            Parse.skipSpaces
            fmap Just parse
        return (ExposedModule m reexport)

instance Binary ExposedModule

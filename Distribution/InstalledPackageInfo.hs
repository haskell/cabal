-- This module is meant to be local-only to Distribution...

module Distribution.InstalledPackageInfo where

import Distribution.Misc(License(..), Dependency, Opt, LocalBuildInfo)
import Distribution.Package(PackageIdentifier(..), PackageConfig)

data InstalledPackageInfo
   = InstalledPackageInfo {
        pkgIdent        :: PackageIdentifier,
        license         :: License,
        copyright       :: String,
        maintainer      :: String,
        stability       :: String,
        auto            :: Bool,
        importDirs     :: [FilePath],
        sourceDirs     :: [FilePath],
        libraryDirs    :: [FilePath],
        hsLibraries    :: [String],
        extraLibraries :: [String],
        includeDirs    :: [FilePath],
        cIncludes      :: [String],
        depends         :: [Dependency], -- use dependencies
        extraHugsOpts :: [Opt],
        extraCcOpts   :: [Opt],
        extraLdOpts   :: [Opt],
        frameworkDirs  :: [FilePath],
        extraFrameworks:: [String]}
    deriving (Read, Show)

emptyInstalledPackageInfo :: InstalledPackageInfo
emptyInstalledPackageInfo = InstalledPackageInfo (PackageIdentifier "" (error "no version"))
                   AllRightsReserved "" "" "" False [] [] [] [] [] []
                   [] [] [] [] [] [] []

getUseInfo :: LocalBuildInfo -> PackageConfig -> InstalledPackageInfo
getUseInfo _ _ = emptyInstalledPackageInfo -- FIX

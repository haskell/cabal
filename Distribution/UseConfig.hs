-- This module is meant to be local-only to Distribution...

module Distribution.UseConfig where

import Distribution.Misc(License(..), Dependency, Opt, LocalBuildInfo)
import Distribution.Package(PackageIdentifier(..), PackageConfig)
import Distribution.Version(Version(NoVersion))

data UseConfig
   = UseConfig {
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

emptyUseConfig :: UseConfig
emptyUseConfig = UseConfig (PackageIdentifier "" NoVersion)
                   AllRightsReserved "" "" "" False [] [] [] [] [] []
                   [] [] [] [] [] [] []

getUseInfo :: LocalBuildInfo -> PackageConfig -> UseConfig
getUseInfo _ _ = emptyUseConfig -- FIX

-- This module is meant to be local-only to Distribution...

module UseConfig where

data UseConfig
   = UseConfig {
        pkgIdent        :: PkgIdentifier,
        license         :: License,
        copyright       :: String,
        maintainer      :: String,
        stability       :: String,
        auto            :: Bool,
--         provides        :: [String],
{- A bit pi-in-the-sky; might indicate that this package provides
    functionality that other packages also provide, such as a compiler
    or GUI framework, and upon which other packages might depend. -}

--         isDefault       :: Bool,
-- might indicate if this is the default compiler or GUI framework.

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

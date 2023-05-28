-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the Paths_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find their version number and find any installed data files
-- at runtime. This code should probably be split off into another module.
module Distribution.Simple.Build.PathsModule
  ( generatePathsModule
  , pkgPathEnvVar
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (shortRelativePath)
import Distribution.System
import Distribution.Version

import qualified Distribution.Simple.Build.PathsModule.Z as Z

-- ------------------------------------------------------------

-- * Building Paths_<pkg>.hs

-- ------------------------------------------------------------

generatePathsModule :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> String
generatePathsModule pkg_descr lbi clbi =
  Z.render
    Z.Z
      { Z.zPackageName = packageName pkg_descr
      , Z.zVersionDigits = show $ versionNumbers $ packageVersion pkg_descr
      , Z.zSupportsCpp = supports_cpp
      , Z.zSupportsNoRebindableSyntax = supports_rebindable_syntax
      , Z.zAbsolute = absolute
      , Z.zRelocatable = relocatable lbi
      , Z.zIsWindows = isWindows
      , Z.zIsI386 = buildArch == I386
      , Z.zIsX8664 = buildArch == X86_64
      , Z.zNot = not
      , Z.zManglePkgName = showPkgName
      , Z.zPrefix = show flat_prefix
      , Z.zBindir = zBindir
      , Z.zLibdir = zLibdir
      , Z.zDynlibdir = zDynlibdir
      , Z.zDatadir = zDatadir
      , Z.zLibexecdir = zLibexecdir
      , Z.zSysconfdir = zSysconfdir
      }
  where
    supports_cpp = supports_language_pragma
    supports_rebindable_syntax = ghc_newer_than (mkVersion [7, 0, 1])
    supports_language_pragma = ghc_newer_than (mkVersion [6, 6, 1])

    ghc_newer_than minVersion =
      case compilerCompatVersion GHC (compiler lbi) of
        Nothing -> False
        Just version -> version `withinRange` orLaterVersion minVersion

    -- In several cases we cannot make relocatable installations
    absolute =
      hasLibs pkg_descr -- we can only make progs relocatable
        || isNothing flat_bindirrel -- if the bin dir is an absolute path
        || not (supportsRelocatableProgs (compilerFlavor (compiler lbi)))

    -- TODO: Here, and with zIsI386 & zIs8664 we should use TARGET platform
    isWindows = case buildOS of
      Windows -> True
      _ -> False

    supportsRelocatableProgs GHC = isWindows
    supportsRelocatableProgs GHCJS = isWindows
    supportsRelocatableProgs _ = False

    cid = componentUnitId clbi

    InstallDirs
      { bindir = flat_bindir
      , libdir = flat_libdir
      , dynlibdir = flat_dynlibdir
      , datadir = flat_datadir
      , libexecdir = flat_libexecdir
      , sysconfdir = flat_sysconfdir
      , prefix = flat_prefix
      } = absoluteInstallCommandDirs pkg_descr lbi cid NoCopyDest

    InstallDirs
      { bindir = flat_bindirrel
      , libdir = flat_libdirrel
      , dynlibdir = flat_dynlibdirrel
      , datadir = flat_datadirrel
      , libexecdir = flat_libexecdirrel
      , sysconfdir = flat_sysconfdirrel
      } = prefixRelativeComponentInstallDirs (packageId pkg_descr) lbi cid

    zBindir, zLibdir, zDynlibdir, zDatadir, zLibexecdir, zSysconfdir :: String
    (zBindir, zLibdir, zDynlibdir, zDatadir, zLibexecdir, zSysconfdir)
      | relocatable lbi =
          ( show flat_bindir_reloc
          , show flat_libdir_reloc
          , show flat_dynlibdir_reloc
          , show flat_datadir_reloc
          , show flat_libexecdir_reloc
          , show flat_sysconfdir_reloc
          )
      | absolute =
          ( show flat_bindir
          , show flat_libdir
          , show flat_dynlibdir
          , show flat_datadir
          , show flat_libexecdir
          , show flat_sysconfdir
          )
      | isWindows =
          ( "maybe (error \"PathsModule.generate\") id (" ++ show flat_bindirrel ++ ")"
          , mkGetDir flat_libdir flat_libdirrel
          , mkGetDir flat_dynlibdir flat_dynlibdirrel
          , mkGetDir flat_datadir flat_datadirrel
          , mkGetDir flat_libexecdir flat_libexecdirrel
          , mkGetDir flat_sysconfdir flat_sysconfdirrel
          )
      | otherwise =
          error "panic! generatePathsModule: should never happen"

    mkGetDir :: FilePath -> Maybe FilePath -> String
    mkGetDir _ (Just dirrel) = "getPrefixDirRel " ++ show dirrel
    mkGetDir dir Nothing = "return " ++ show dir

    flat_bindir_reloc = shortRelativePath flat_prefix flat_bindir
    flat_libdir_reloc = shortRelativePath flat_prefix flat_libdir
    flat_dynlibdir_reloc = shortRelativePath flat_prefix flat_dynlibdir
    flat_datadir_reloc = shortRelativePath flat_prefix flat_datadir
    flat_libexecdir_reloc = shortRelativePath flat_prefix flat_libexecdir
    flat_sysconfdir_reloc = shortRelativePath flat_prefix flat_sysconfdir

-- | Generates the name of the environment variable controlling the path
-- component of interest.
--
-- Note: The format of these strings is part of Cabal's public API;
-- changing this function constitutes a *backwards-compatibility* break.
pkgPathEnvVar
  :: PackageDescription
  -> String
  -- ^ path component; one of \"bindir\", \"libdir\", -- \"datadir\", \"libexecdir\", or \"sysconfdir\"
  -> String
  -- ^ environment variable name
pkgPathEnvVar pkg_descr var =
  showPkgName (packageName pkg_descr) ++ "_" ++ var

showPkgName :: PackageName -> String
showPkgName = map fixchar . unPackageName

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c = c

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Build.PackageInfoModule
-- Copyright   :
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the PackageInfo_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find their package informations.
module Distribution.Simple.Build.PackageInfoModule
  ( generatePackageInfoModule
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Pretty ( prettyShow )
import Distribution.Utils.ShortText
import Distribution.Version

import qualified Distribution.Simple.Build.PackageInfoModule.Z as Z

-- ------------------------------------------------------------

-- * Building Paths_<pkg>.hs

-- ------------------------------------------------------------

generatePackageInfoModule :: PackageDescription -> LocalBuildInfo -> String
generatePackageInfoModule pkg_descr lbi =
  Z.render
    Z.Z
      { Z.zPackageName = packageName pkg_descr
      , Z.zVersionDigits = show $ versionNumbers $ packageVersion pkg_descr
      , Z.zSynopsis = show $ fromShortText $ synopsis pkg_descr
      , Z.zCopyright = show $ fromShortText $ copyright pkg_descr
      , Z.zLicense = show $ prettyShow $ license pkg_descr
      , Z.zHomepage = show $ fromShortText $ homepage pkg_descr
      , Z.zSupportsNoRebindableSyntax = supports_rebindable_syntax
      , Z.zManglePkgName = showPkgName
      , Z.zShow = show
      }
  where
    supports_rebindable_syntax = ghc_newer_than (mkVersion [7, 0, 1])

    ghc_newer_than minVersion =
      case compilerCompatVersion GHC (compiler lbi) of
        Nothing -> False
        Just version -> version `withinRange` orLaterVersion minVersion

showPkgName :: PackageName -> String
showPkgName = map fixchar . unPackageName

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c = c

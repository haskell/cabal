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
-- enables them to find their package information.
module Distribution.Simple.Build.PackageInfoModule
  ( generatePackageInfoModule
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Package
  ( PackageName
  , packageName
  , packageVersion
  , unPackageNameST
  )
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.Version (versionNumbers)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Distribution.Simple.Build.PackageInfoModule.Z as Z

-- ------------------------------------------------------------

-- * Building Paths_<pkg>.hs

-- ------------------------------------------------------------

generatePackageInfoModule :: PackageDescription -> String
generatePackageInfoModule pkg_descr =
  Z.render
    Z.Z
      { Z.zPackageName = showPkgName $ packageName pkg_descr
      , Z.zVersionDigits = Text.pack $ show $ versionNumbers $ packageVersion pkg_descr
      , Z.zSynopsis = synopsis pkg_descr
      , Z.zCopyright = copyright pkg_descr
      , Z.zHomepage = homepage pkg_descr
      }

showPkgName :: PackageName -> Text
showPkgName = Text.map fixchar . unPackageNameST

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c = c

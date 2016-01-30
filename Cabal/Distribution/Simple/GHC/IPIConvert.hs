-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.IPI642
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Helper functions for 'Distribution.Simple.GHC.IPI642'.
module Distribution.Simple.GHC.IPIConvert (
    PackageIdentifier, convertPackageId,
    License, convertLicense,
    convertModuleName
  ) where

import qualified Distribution.Package as Current hiding (installedUnitId)
import qualified Distribution.License as Current

import Distribution.Version
import Distribution.ModuleName
import Distribution.Text

import Data.Maybe

data PackageIdentifier = PackageIdentifier {
    pkgName    :: String,
    pkgVersion :: Version
  }
  deriving Read

convertPackageId :: PackageIdentifier -> Current.PackageIdentifier
convertPackageId PackageIdentifier { pkgName = n, pkgVersion = v } =
  Current.PackageIdentifier (Current.PackageName n) v

data License = GPL | LGPL | BSD3 | BSD4
             | PublicDomain | AllRightsReserved | OtherLicense
  deriving Read

convertModuleName :: String -> ModuleName
convertModuleName s = fromJust $ simpleParse s

convertLicense :: License -> Current.License
convertLicense GPL  = Current.GPL  Nothing
convertLicense LGPL = Current.LGPL Nothing
convertLicense BSD3 = Current.BSD3
convertLicense BSD4 = Current.BSD4
convertLicense PublicDomain = Current.PublicDomain
convertLicense AllRightsReserved = Current.AllRightsReserved
convertLicense OtherLicense = Current.OtherLicense

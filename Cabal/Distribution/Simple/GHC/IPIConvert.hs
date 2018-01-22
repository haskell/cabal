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

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Types.PackageId as Current
import qualified Distribution.Types.PackageName as Current
import qualified Distribution.License as Current
import qualified Distribution.SPDX as SPDX

import Distribution.Version
import Distribution.ModuleName
import Distribution.Text

-- | This is a indeed a munged package id, but the constructor name cannot be
-- changed or the Read instance (the entire point of this type) will break.
data PackageIdentifier = PackageIdentifier {
    pkgName    :: String,
    pkgVersion :: Version
  }
  deriving Read

convertPackageId :: PackageIdentifier -> Current.PackageId
convertPackageId PackageIdentifier { pkgName = n, pkgVersion = v } =
  Current.PackageIdentifier (Current.mkPackageName n) v

data License = GPL | LGPL | BSD3 | BSD4
             | PublicDomain | AllRightsReserved | OtherLicense
  deriving Read

convertModuleName :: String -> ModuleName
convertModuleName s = fromMaybe (error "convertModuleName") $ simpleParse s

convertLicense :: License -> Either SPDX.License Current.License
convertLicense GPL               = Right $ Current.GPL  Nothing
convertLicense LGPL              = Right $ Current.LGPL Nothing
convertLicense BSD3              = Right $ Current.BSD3
convertLicense BSD4              = Right $ Current.BSD4
convertLicense PublicDomain      = Right $ Current.PublicDomain
convertLicense AllRightsReserved = Right $ Current.AllRightsReserved
convertLicense OtherLicense      = Right $ Current.OtherLicense

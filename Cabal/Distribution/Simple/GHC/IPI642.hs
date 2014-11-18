-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.IPI642
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--

module Distribution.Simple.GHC.IPI642 (
    InstalledPackageInfo(..),
    toCurrent,

    -- Don't use these, they're only for conversion purposes
    PackageIdentifier, convertPackageId,
    License, convertLicense,
    convertModuleName
  ) where

import qualified Distribution.InstalledPackageInfo as Current
import qualified Distribution.Package as Current hiding (depends, installedPackageId)
import qualified Distribution.License as Current

import Distribution.Version (Version)
import Distribution.ModuleName (ModuleName)
import Distribution.Text (simpleParse,display)

import Data.Maybe

-- | This is the InstalledPackageInfo type used by ghc-6.4.2 and later.
--
-- It's here purely for the 'Read' instance so that we can read the package
-- database used by those ghc versions. It is a little hacky to read the
-- package db directly, but we do need the info and until ghc-6.9 there was
-- no better method.
--
-- In ghc-6.4.1 and before the format was slightly different.
-- See "Distribution.Simple.GHC.IPI642"
--
data InstalledPackageInfo = InstalledPackageInfo {
    package           :: PackageIdentifier,
    license           :: License,
    copyright         :: String,
    maintainer        :: String,
    author            :: String,
    stability         :: String,
    homepage          :: String,
    pkgUrl            :: String,
    description       :: String,
    category          :: String,
    exposed           :: Bool,
    exposedModules    :: [String],
    hiddenModules     :: [String],
    importDirs        :: [FilePath],
    libraryDirs       :: [FilePath],
    hsLibraries       :: [String],
    extraLibraries    :: [String],
    extraGHCiLibraries:: [String],
    includeDirs       :: [FilePath],
    includes          :: [String],
    depends           :: [PackageIdentifier],
    hugsOptions       :: [String],
    ccOptions         :: [String],
    ldOptions         :: [String],
    frameworkDirs     :: [FilePath],
    frameworks        :: [String],
    haddockInterfaces :: [FilePath],
    haddockHTMLs      :: [FilePath]
  }
  deriving Read

data PackageIdentifier = PackageIdentifier {
    pkgName    :: String,
    pkgVersion :: Version
  }
  deriving Read

data License = GPL | LGPL | BSD3 | BSD4
             | PublicDomain | AllRightsReserved | OtherLicense
  deriving Read

convertPackageId :: PackageIdentifier -> Current.PackageIdentifier
convertPackageId PackageIdentifier { pkgName = n, pkgVersion = v } =
  Current.PackageIdentifier (Current.PackageName n) v

mkInstalledPackageId :: Current.PackageIdentifier -> Current.InstalledPackageId
mkInstalledPackageId = Current.InstalledPackageId . display

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

toCurrent :: InstalledPackageInfo -> Current.InstalledPackageInfo
toCurrent ipi@InstalledPackageInfo{} =
  let pid = convertPackageId (package ipi)
      mkExposedModule m = Current.ExposedModule m Nothing Nothing
  in Current.InstalledPackageInfo {
    Current.installedPackageId = mkInstalledPackageId (convertPackageId (package ipi)),
    Current.sourcePackageId    = pid,
    Current.packageKey         = Current.OldPackageKey pid,
    Current.license            = convertLicense (license ipi),
    Current.copyright          = copyright ipi,
    Current.maintainer         = maintainer ipi,
    Current.author             = author ipi,
    Current.stability          = stability ipi,
    Current.homepage           = homepage ipi,
    Current.pkgUrl             = pkgUrl ipi,
    Current.synopsis           = "",
    Current.description        = description ipi,
    Current.category           = category ipi,
    Current.exposed            = exposed ipi,
    Current.exposedModules     = map (mkExposedModule . convertModuleName) (exposedModules ipi),
    Current.hiddenModules      = map convertModuleName (hiddenModules ipi),
    Current.trusted            = Current.trusted Current.emptyInstalledPackageInfo,
    Current.importDirs         = importDirs ipi,
    Current.libraryDirs        = libraryDirs ipi,
    Current.dataDir            = "",
    Current.hsLibraries        = hsLibraries ipi,
    Current.extraLibraries     = extraLibraries ipi,
    Current.extraGHCiLibraries = extraGHCiLibraries ipi,
    Current.includeDirs        = includeDirs ipi,
    Current.includes           = includes ipi,
    Current.depends            = map (mkInstalledPackageId.convertPackageId) (depends ipi),
    Current.ccOptions          = ccOptions ipi,
    Current.ldOptions          = ldOptions ipi,
    Current.frameworkDirs      = frameworkDirs ipi,
    Current.frameworks         = frameworks ipi,
    Current.haddockInterfaces  = haddockInterfaces ipi,
    Current.haddockHTMLs       = haddockHTMLs ipi
  }

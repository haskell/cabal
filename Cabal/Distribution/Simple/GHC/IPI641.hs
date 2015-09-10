-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.IPI641
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--

module Distribution.Simple.GHC.IPI641 (
    InstalledUnitInfo(..),
    toCurrent,
  ) where

import qualified Distribution.InstalledUnitInfo as Current
import qualified Distribution.Package as Current hiding (installedUnitId)
import Distribution.Text (display)

import Distribution.Simple.GHC.IPI642
         ( PackageIdentifier, convertPackageId
         , License, convertLicense, convertModuleName )

-- | This is the InstalledUnitInfo type used by ghc-6.4 and 6.4.1.
--
-- It's here purely for the 'Read' instance so that we can read the package
-- database used by those ghc versions. It is a little hacky to read the
-- package db directly, but we do need the info and until ghc-6.9 there was
-- no better method.
--
-- In ghc-6.4.2 the format changed a bit. See "Distribution.Simple.GHC.IPI642"
--
data InstalledUnitInfo = InstalledUnitInfo {
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

mkInstalledPackageId :: Current.PackageIdentifier -> Current.InstalledPackageId
mkInstalledPackageId = Current.InstalledPackageId . display

mkInstalledUnitId :: Current.PackageIdentifier -> Current.InstalledUnitId
mkInstalledUnitId = Current.InstalledUnitId . display

toCurrent :: InstalledUnitInfo -> Current.InstalledUnitInfo
toCurrent ipi@InstalledUnitInfo{} =
  let pid = convertPackageId (package ipi)
      mkExposedModule m = Current.ExposedModule m Nothing Nothing
  in Current.InstalledUnitInfo {
    Current.installedPackageId = mkInstalledPackageId pid,
    Current.sourcePackageId    = pid,
    Current.installedUnitId         = mkInstalledUnitId pid,
    Current.compatPackageKey   = mkInstalledUnitId pid,
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
    Current.abiHash            = Current.AbiHash "",
    Current.exposed            = exposed ipi,
    Current.exposedModules     = map (mkExposedModule . convertModuleName) (exposedModules ipi),
    Current.instantiatedWith   = [],
    Current.hiddenModules      = map convertModuleName (hiddenModules ipi),
    Current.trusted            = Current.trusted Current.emptyInstalledUnitInfo,
    Current.importDirs         = importDirs ipi,
    Current.libraryDirs        = libraryDirs ipi,
    Current.dataDir            = "",
    Current.hsLibraries        = hsLibraries ipi,
    Current.extraLibraries     = extraLibraries ipi,
    Current.extraGHCiLibraries = [],
    Current.includeDirs        = includeDirs ipi,
    Current.includes           = includes ipi,
    Current.depends            = map (mkInstalledUnitId.convertPackageId) (depends ipi),
    Current.ccOptions          = ccOptions ipi,
    Current.ldOptions          = ldOptions ipi,
    Current.frameworkDirs      = frameworkDirs ipi,
    Current.frameworks         = frameworks ipi,
    Current.haddockInterfaces  = haddockInterfaces ipi,
    Current.haddockHTMLs       = haddockHTMLs ipi,
    Current.pkgRoot            = Nothing
  }

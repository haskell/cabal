-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC.IPI642
-- Copyright   :  (c) The University of Glasgow 2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the University nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.GHC.IPI642 (
    InstalledPackageInfo,
    toCurrent,

    -- Don't use these, they're only for conversion purposes
    PackageIdentifier, convertPackageId,
    License, convertLicense,
    convertModuleName
  ) where

import qualified Distribution.InstalledPackageInfo as Current
import qualified Distribution.Package as Current hiding (depends)
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
toCurrent ipi@InstalledPackageInfo{} = Current.InstalledPackageInfo {
    Current.installedPackageId = mkInstalledPackageId (convertPackageId (package ipi)),
    Current.sourcePackageId    = convertPackageId (package ipi),
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
    Current.exposedModules     = map convertModuleName (exposedModules ipi),
    Current.hiddenModules      = map convertModuleName (hiddenModules ipi),
    Current.trusted            = False,
    Current.importDirs         = importDirs ipi,
    Current.libraryDirs        = libraryDirs ipi,
    Current.hsLibraries        = hsLibraries ipi,
    Current.extraLibraries     = extraLibraries ipi,
    Current.extraGHCiLibraries = extraGHCiLibraries ipi,
    Current.includeDirs        = includeDirs ipi,
    Current.includes           = includes ipi,
    Current.depends            = map (mkInstalledPackageId.convertPackageId) (depends ipi),
    Current.hugsOptions        = hugsOptions ipi,
    Current.ccOptions          = ccOptions ipi,
    Current.ldOptions          = ldOptions ipi,
    Current.frameworkDirs      = frameworkDirs ipi,
    Current.frameworks         = frameworks ipi,
    Current.haddockInterfaces  = haddockInterfaces ipi,
    Current.haddockHTMLs       = haddockHTMLs ipi
  }

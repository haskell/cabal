-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Register
-- Copyright   :  Isaac Jones 2003-2004
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with registering and unregistering packages. There are a
-- couple ways it can do this, one is to do it directly. Another is to generate
-- a script that can be run later to do it. The idea here being that the user
-- is shielded from the details of what command to use for package registration
-- for a particular compiler. In practice this aspect was not especially
-- popular so we also provide a way to simply generate the package registration
-- file which then must be manually passed to @ghc-pkg@. It is possible to
-- generate registration information for where the package is to be installed,
-- or alternatively to register the package inplace in the build tree. The
-- latter is occasionally handy, and will become more important when we try to
-- build multi-package systems.
--
-- This module does not delegate anything to the per-compiler modules but just
-- mixes it all in in this module, which is rather unsatisfactory. The script
-- generation and the unregister feature are not well used or tested.

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
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

module Distribution.Simple.Register (
    register,
    unregister,

    registerPackage,
    inplaceInstalledPackageInfo,
    absoluteInstalledPackageInfo,
    generalInstalledPackageInfo,
  ) where

import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , InstallDirs(..), absoluteInstallDirs )
import Distribution.Simple.BuildPaths (haddockName)
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor
         , PackageDB(..), registrationPackageDB )
import Distribution.Simple.Program
         ( ConfiguredProgram, runProgramInvocation
         , requireProgram, lookupProgram, ghcPkgProgram, lhcPkgProgram )
import Distribution.Simple.Program.Script
         ( invocationAsSystemScript )
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.Simple.Setup
         ( RegisterFlags(..), CopyDest(..)
         , fromFlag, fromFlagOrDefault, flagToMaybe )
import Distribution.PackageDescription
         ( PackageDescription(..), Library(..), BuildInfo(..), hcOptions )
import Distribution.Package
         ( Package(..), packageName )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, InstalledPackageInfo_(InstalledPackageInfo)
         , showInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, writeFileAtomic
         , die, notice, setupMessage )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Text
         ( display )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Compat.CopyFile
         ( setFileExecutable )

import System.FilePath ((</>), (<.>), isAbsolute)
import System.Directory
         ( getCurrentDirectory, removeDirectoryRecursive )
import System.IO.Error (try)

import Control.Monad (when)
import Data.Maybe
         ( isJust, fromMaybe )
import Data.List (partition)


-- -----------------------------------------------------------------------------
-- Registration

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg@PackageDescription { library       = Just lib  }
         lbi@LocalBuildInfo     { libraryConfig = Just clbi } regFlags
  -- Three different modes:
  | modeGenerateRegFile   = writeRegistrationFile
  | modeGenerateRegScript = writeRegisterScript
  | otherwise             = registerPackage verbosity
                              pkg lib lbi clbi distPref inplace packageDb

  where
    modeGenerateRegFile = isJust (flagToMaybe (regGenPkgConf regFlags))
    regFile             = fromMaybe (display (packageId pkg) <.> "conf")
                                    (fromFlag (regGenPkgConf regFlags))

    modeGenerateRegScript = fromFlag (regGenScript regFlags)

    inplace   = fromFlag (regInPlace regFlags)
    packageDb = fromFlagOrDefault (registrationPackageDB (withPackageDB lbi))
                                  (regPackageDB regFlags)
    distPref  = fromFlag (regDistPref regFlags)
    verbosity = fromFlag (regVerbosity regFlags)

    writeRegistrationFile = do
      installedPkgInfo <- generateRegistrationInfo
                            pkg lib lbi clbi inplace distPref
      notice verbosity ("Creating package registration file: " ++ regFile)
      writeFileAtomic regFile (showInstalledPackageInfo installedPkgInfo ++ "\n")

    writeRegisterScript =
      case compilerFlavor (compiler lbi) of
        GHC  -> do (ghcPkg, _) <- requireProgram verbosity ghcPkgProgram (withPrograms lbi)
                   writeHcPkgRegisterScript verbosity ghcPkg pkg lib lbi clbi distPref inplace packageDb
        LHC  -> do (lhcPkg, _) <- requireProgram verbosity lhcPkgProgram (withPrograms lbi)
                   writeHcPkgRegisterScript verbosity lhcPkg pkg lib lbi clbi distPref inplace packageDb
        Hugs -> notice verbosity "Registration scripts not needed for hugs"
        JHC  -> notice verbosity "Registration scripts not needed for jhc"
        NHC  -> notice verbosity "Registration scripts not needed for nhc98"
        _    -> die "Registration scripts are not implemented for this compiler"

register _ _ regFlags = notice verbosity "No package to register"
  where
    verbosity = fromFlag (regVerbosity regFlags)


generateRegistrationInfo :: PackageDescription
                         -> Library
                         -> LocalBuildInfo
                         -> ComponentLocalBuildInfo
                         -> Bool
                         -> FilePath
                         -> IO InstalledPackageInfo
generateRegistrationInfo pkg lib lbi clbi inplace distPref = do
  --TODO: eliminate pwd!
  pwd <- getCurrentDirectory
  let installedPkgInfo
        | inplace   = inplaceInstalledPackageInfo pwd distPref
                        pkg lib lbi clbi
        | otherwise = absoluteInstalledPackageInfo
                        pkg lib lbi clbi
  return installedPkgInfo


registerPackage :: Verbosity
                -> PackageDescription
                -> Library
                -> LocalBuildInfo
                -> ComponentLocalBuildInfo
                -> FilePath
                -> Bool
                -> PackageDB
                -> IO ()
registerPackage verbosity pkg lib lbi clbi distPref inplace packageDb = do
  setupMessage verbosity "Registering" (packageId pkg)
  case compilerFlavor (compiler lbi) of
    GHC  -> registerPackageGHC  verbosity pkg lib lbi clbi distPref inplace packageDb
    LHC  -> registerPackageLHC  verbosity pkg lib lbi clbi distPref inplace packageDb
    Hugs -> registerPackageHugs verbosity pkg lib lbi clbi distPref inplace packageDb
    JHC  -> notice verbosity "Registering for jhc (nothing to do)"
    NHC  -> notice verbosity "Registering for nhc98 (nothing to do)"
    _    -> die "Registering is not implemented for this compiler"


registerPackageGHC, registerPackageLHC, registerPackageHugs
  :: Verbosity
  -> PackageDescription
  -> Library
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> FilePath
  -> Bool
  -> PackageDB
  -> IO ()
registerPackageGHC verbosity pkg lib lbi clbi distPref inplace packageDb = do
  installedPkgInfo <- generateRegistrationInfo
                        pkg lib lbi clbi inplace distPref
  let Just ghcPkg = lookupProgram ghcPkgProgram (withPrograms lbi)
  HcPkg.reregister verbosity ghcPkg packageDb (Right installedPkgInfo)


registerPackageLHC verbosity pkg lib lbi clbi distPref inplace packageDb = do
  installedPkgInfo <- generateRegistrationInfo
                        pkg lib lbi clbi inplace distPref
  let Just lhcPkg = lookupProgram lhcPkgProgram (withPrograms lbi)
  HcPkg.reregister verbosity lhcPkg packageDb (Right installedPkgInfo)


registerPackageHugs verbosity pkg lib lbi clbi distPref inplace _packageDb = do
  when inplace $ die "--inplace is not supported with Hugs"
  installedPkgInfo <- generateRegistrationInfo
                        pkg lib lbi clbi inplace distPref
  let installDirs = absoluteInstallDirs pkg lbi NoCopyDest
  createDirectoryIfMissingVerbose verbosity True (libdir installDirs)
  writeFileAtomic (libdir installDirs </> "package.conf")
                  (showInstalledPackageInfo installedPkgInfo ++ "\n")


writeHcPkgRegisterScript :: Verbosity
                         -> ConfiguredProgram
                         -> PackageDescription
                         -> Library
                         -> LocalBuildInfo
                         -> ComponentLocalBuildInfo
                         -> FilePath
                         -> Bool
                         -> PackageDB
                         -> IO ()
writeHcPkgRegisterScript verbosity hcPkg pkg lib lbi clbi distPref inplace packageDb = do
  installedPkgInfo <- generateRegistrationInfo
                        pkg lib lbi clbi inplace distPref

  let invocation  = HcPkg.reregisterInvocation hcPkg Verbosity.normal
                      packageDb (Right installedPkgInfo)
      regScript   = invocationAsSystemScript buildOS   invocation

  notice verbosity ("Creating package registration script: " ++ regScriptFileName)
  writeFileAtomic regScriptFileName regScript
  setFileExecutable regScriptFileName

regScriptFileName :: FilePath
regScriptFileName = case buildOS of
                        Windows -> "register.bat"
                        _       -> "register.sh"


-- -----------------------------------------------------------------------------
-- Making the InstalledPackageInfo

-- | Construct 'InstalledPackageInfo' for a library in a package, given a set
-- of installation directories.
--
generalInstalledPackageInfo
  :: ([FilePath] -> [FilePath]) -- ^ Translate relative include dir paths to
                                -- absolute paths.
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> InstallDirs FilePath
  -> InstalledPackageInfo
generalInstalledPackageInfo adjustRelIncDirs pkg lib clbi installDirs =
  InstalledPackageInfo {
    IPI.package            = packageId   pkg,
    IPI.license            = license     pkg,
    IPI.copyright          = copyright   pkg,
    IPI.maintainer         = maintainer  pkg,
    IPI.author             = author      pkg,
    IPI.stability          = stability   pkg,
    IPI.homepage           = homepage    pkg,
    IPI.pkgUrl             = pkgUrl      pkg,
    IPI.description        = description pkg,
    IPI.category           = category    pkg,
    IPI.exposed            = libExposed  lib,
    IPI.exposedModules     = exposedModules lib,
    IPI.hiddenModules      = otherModules bi,
    IPI.importDirs         = [ libdir installDirs | hasModules ],
    IPI.libraryDirs        = if hasLibrary
                               then libdir installDirs : extraLibDirs bi
                               else                      extraLibDirs bi,
    IPI.hsLibraries        = [ "HS" ++ display (packageId pkg) | hasLibrary ],
    IPI.extraLibraries     = extraLibs bi,
    IPI.extraGHCiLibraries = [],
    IPI.includeDirs        = absinc ++ adjustRelIncDirs relinc,
    IPI.includes           = includes bi,
    IPI.depends            = componentPackageDeps clbi,
    IPI.hugsOptions        = hcOptions Hugs bi,
    IPI.ccOptions          = [], -- Note. NOT ccOptions bi!
                                 -- We don't want cc-options to be propagated
                                 -- to C compilations in other packages.
    IPI.ldOptions          = ldOptions bi,
    IPI.frameworkDirs      = [],
    IPI.frameworks         = frameworks bi,
    IPI.haddockInterfaces  = [haddockdir installDirs </> haddockName pkg],
    IPI.haddockHTMLs       = [htmldir installDirs]
  }
  where
    bi = libBuildInfo lib
    (absinc, relinc) = partition isAbsolute (includeDirs bi)
    hasModules = not $ null (exposedModules lib)
                    && null (otherModules bi)
    hasLibrary = hasModules || not (null (cSources bi))


-- | Construct 'InstalledPackageInfo' for a library that is inplace in the
-- build tree.
--
-- This function knows about the layout of inplace packages.
--
inplaceInstalledPackageInfo :: FilePath -- ^ top of the build tree
                            -> FilePath -- ^ location of the dist tree
                            -> PackageDescription
                            -> Library
                            -> LocalBuildInfo
                            -> ComponentLocalBuildInfo
                            -> InstalledPackageInfo
inplaceInstalledPackageInfo inplaceDir distPref pkg lib lbi clbi =
    generalInstalledPackageInfo adjustReativeIncludeDirs pkg lib clbi installDirs
  where
    adjustReativeIncludeDirs = map (inplaceDir </>)
    installDirs =
      (absoluteInstallDirs pkg lbi NoCopyDest) {
        libdir     = inplaceDir </> buildDir lbi,
        datadir    = inplaceDir,
        datasubdir = distPref,
        docdir     = inplaceDocdir,
        htmldir    = inplaceHtmldir,
        haddockdir = inplaceHtmldir
      }
    inplaceDocdir  = inplaceDir </> distPref </> "doc"
    inplaceHtmldir = inplaceDocdir </> "html" </> display (packageName pkg)


-- | Construct 'InstalledPackageInfo' for the final install location of a
-- library package.
--
-- This function knows about the layout of installed packages.
--
absoluteInstalledPackageInfo :: PackageDescription
                             -> Library
                             -> LocalBuildInfo
                             -> ComponentLocalBuildInfo
                             -> InstalledPackageInfo
absoluteInstalledPackageInfo pkg lib lbi clbi =
    generalInstalledPackageInfo adjustReativeIncludeDirs pkg lib clbi installDirs
  where
    -- For installed packages we install all include files into one dir,
    -- whereas in the build tree they may live in multiple local dirs.
    adjustReativeIncludeDirs _
      | null (installIncludes bi) = []
      | otherwise                 = [includedir installDirs]
    bi = libBuildInfo lib
    installDirs = absoluteInstallDirs pkg lbi NoCopyDest


-- -----------------------------------------------------------------------------
-- Unregistration

unregister :: PackageDescription -> LocalBuildInfo -> RegisterFlags -> IO ()
unregister pkg lbi regFlags = do
  let pkgid     = packageId pkg
      genScript = fromFlag (regGenScript regFlags)
      verbosity = fromFlag (regVerbosity regFlags)
      packageDb = fromFlagOrDefault (registrationPackageDB (withPackageDB lbi))
                                    (regPackageDB regFlags)
      installDirs = absoluteInstallDirs pkg lbi NoCopyDest
  setupMessage verbosity "Unregistering" pkgid
  case compilerFlavor (compiler lbi) of
    GHC ->
      let Just ghcPkg = lookupProgram ghcPkgProgram (withPrograms lbi)
          invocation = HcPkg.unregisterInvocation ghcPkg Verbosity.normal
                         packageDb pkgid
      in if genScript
           then writeFileAtomic unregScriptFileName
                  (invocationAsSystemScript buildOS invocation)
            else runProgramInvocation verbosity invocation
    Hugs -> do
        try $ removeDirectoryRecursive (libdir installDirs)
        return ()
    NHC -> do
        try $ removeDirectoryRecursive (libdir installDirs)
        return ()
    _ ->
        die ("only unregistering with GHC and Hugs is implemented")

unregScriptFileName :: FilePath
unregScriptFileName = case buildOS of
                          Windows -> "unregister.bat"
                          _       -> "unregister.sh"

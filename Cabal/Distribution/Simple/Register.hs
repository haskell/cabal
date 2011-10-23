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
    generateRegistrationInfo,
    inplaceInstalledPackageInfo,
    absoluteInstalledPackageInfo,
    generalInstalledPackageInfo,
  ) where

import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), ComponentLocalBuildInfo(..)
         , InstallDirs(..), absoluteInstallDirs )
import Distribution.Simple.BuildPaths (haddockName)
import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.LHC  as LHC
import qualified Distribution.Simple.Hugs as Hugs
import qualified Distribution.Simple.UHC  as UHC
import Distribution.Simple.Compiler
         ( compilerVersion, CompilerFlavor(..), compilerFlavor
         , PackageDBStack, registrationPackageDB )
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
         ( Package(..), packageName, InstalledPackageId(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo, InstalledPackageInfo_(InstalledPackageInfo)
         , showInstalledPackageInfo )
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Simple.Utils
         ( writeUTF8File, writeFileAtomic, setFileExecutable
         , die, notice, setupMessage )
import Distribution.System
         ( OS(..), buildOS )
import Distribution.Text
         ( display )
import Distribution.Version ( Version(..) )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Compat.Exception
         ( tryIO )

import System.FilePath ((</>), (<.>), isAbsolute)
import System.Directory
         ( getCurrentDirectory, removeDirectoryRecursive )

import Data.Maybe
         ( isJust, fromMaybe, maybeToList )
import Data.List
         ( partition, nub )


-- -----------------------------------------------------------------------------
-- Registration

register :: PackageDescription -> LocalBuildInfo
         -> RegisterFlags -- ^Install in the user's database?; verbose
         -> IO ()
register pkg@PackageDescription { library       = Just lib  }
         lbi@LocalBuildInfo     { libraryConfig = Just clbi } regFlags
  = do

    installedPkgInfo <- generateRegistrationInfo
                           verbosity pkg lib lbi clbi inplace distPref

     -- Three different modes:
    case () of
     _ | modeGenerateRegFile   -> writeRegistrationFile installedPkgInfo
       | modeGenerateRegScript -> writeRegisterScript   installedPkgInfo
       | otherwise             -> registerPackage verbosity
                                    installedPkgInfo pkg lbi inplace packageDbs

  where
    modeGenerateRegFile = isJust (flagToMaybe (regGenPkgConf regFlags))
    regFile             = fromMaybe (display (packageId pkg) <.> "conf")
                                    (fromFlag (regGenPkgConf regFlags))

    modeGenerateRegScript = fromFlag (regGenScript regFlags)

    inplace   = fromFlag (regInPlace regFlags)
    -- FIXME: there's really no guarantee this will work.
    -- registering into a totally different db stack can
    -- fail if dependencies cannot be satisfied.
    packageDbs = nub $ withPackageDB lbi
                    ++ maybeToList (flagToMaybe  (regPackageDB regFlags))
    distPref  = fromFlag (regDistPref regFlags)
    verbosity = fromFlag (regVerbosity regFlags)

    writeRegistrationFile installedPkgInfo = do
      notice verbosity ("Creating package registration file: " ++ regFile)
      writeUTF8File regFile (showInstalledPackageInfo installedPkgInfo)

    writeRegisterScript installedPkgInfo =
      case compilerFlavor (compiler lbi) of
        GHC  -> do (ghcPkg, _) <- requireProgram verbosity ghcPkgProgram (withPrograms lbi)
                   writeHcPkgRegisterScript verbosity installedPkgInfo ghcPkg packageDbs
        LHC  -> do (lhcPkg, _) <- requireProgram verbosity lhcPkgProgram (withPrograms lbi)
                   writeHcPkgRegisterScript verbosity installedPkgInfo lhcPkg packageDbs
        Hugs -> notice verbosity "Registration scripts not needed for hugs"
        JHC  -> notice verbosity "Registration scripts not needed for jhc"
        NHC  -> notice verbosity "Registration scripts not needed for nhc98"
        UHC  -> notice verbosity "Registration scripts not needed for uhc"
        _    -> die "Registration scripts are not implemented for this compiler"

register _ _ regFlags = notice verbosity "No package to register"
  where
    verbosity = fromFlag (regVerbosity regFlags)


generateRegistrationInfo :: Verbosity
                         -> PackageDescription
                         -> Library
                         -> LocalBuildInfo
                         -> ComponentLocalBuildInfo
                         -> Bool
                         -> FilePath
                         -> IO InstalledPackageInfo
generateRegistrationInfo verbosity pkg lib lbi clbi inplace distPref = do
  --TODO: eliminate pwd!
  pwd <- getCurrentDirectory

  --TODO: the method of setting the InstalledPackageId is compiler specific
  --      this aspect should be delegated to a per-compiler helper.
  let comp = compiler lbi
  ipid <-
    case compilerFlavor comp of
     GHC | compilerVersion comp >= Version [6,11] [] -> do
            s <- GHC.libAbiHash verbosity pkg lbi lib clbi
            return (InstalledPackageId (display (packageId pkg) ++ '-':s))
     _other -> do
            return (InstalledPackageId (display (packageId pkg)))

  let installedPkgInfo
        | inplace   = inplaceInstalledPackageInfo pwd distPref
                        pkg lib lbi clbi
        | otherwise = absoluteInstalledPackageInfo
                        pkg lib lbi clbi

  return installedPkgInfo{ IPI.installedPackageId = ipid }


registerPackage :: Verbosity
                -> InstalledPackageInfo
                -> PackageDescription
                -> LocalBuildInfo
                -> Bool
                -> PackageDBStack
                -> IO ()
registerPackage verbosity installedPkgInfo pkg lbi inplace packageDbs = do
  setupMessage verbosity "Registering" (packageId pkg)
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.registerPackage  verbosity installedPkgInfo pkg lbi inplace packageDbs
    LHC  -> LHC.registerPackage  verbosity installedPkgInfo pkg lbi inplace packageDbs
    Hugs -> Hugs.registerPackage verbosity installedPkgInfo pkg lbi inplace packageDbs
    UHC  -> UHC.registerPackage  verbosity installedPkgInfo pkg lbi inplace packageDbs
    JHC  -> notice verbosity "Registering for jhc (nothing to do)"
    NHC  -> notice verbosity "Registering for nhc98 (nothing to do)"
    _    -> die "Registering is not implemented for this compiler"


writeHcPkgRegisterScript :: Verbosity
                         -> InstalledPackageInfo
                         -> ConfiguredProgram
                         -> PackageDBStack
                         -> IO ()
writeHcPkgRegisterScript verbosity installedPkgInfo hcPkg packageDbs = do
  let invocation  = HcPkg.reregisterInvocation hcPkg Verbosity.normal
                      packageDbs (Right installedPkgInfo)
      regScript   = invocationAsSystemScript buildOS   invocation

  notice verbosity ("Creating package registration script: " ++ regScriptFileName)
  writeUTF8File regScriptFileName regScript
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
    --TODO: do not open-code this conversion from PackageId to InstalledPackageId
    IPI.installedPackageId = InstalledPackageId (display (packageId pkg)),
    IPI.sourcePackageId    = packageId   pkg,
    IPI.license            = license     pkg,
    IPI.copyright          = copyright   pkg,
    IPI.maintainer         = maintainer  pkg,
    IPI.author             = author      pkg,
    IPI.stability          = stability   pkg,
    IPI.homepage           = homepage    pkg,
    IPI.pkgUrl             = pkgUrl      pkg,
    IPI.synopsis           = synopsis    pkg,
    IPI.description        = description pkg,
    IPI.category           = category    pkg,
    IPI.exposed            = libExposed  lib,
    IPI.exposedModules     = exposedModules lib,
    IPI.hiddenModules      = otherModules bi,
    IPI.trusted            = IPI.trusted IPI.emptyInstalledPackageInfo,
    IPI.importDirs         = [ libdir installDirs | hasModules ],
    IPI.libraryDirs        = if hasLibrary
                               then libdir installDirs : extraLibDirs bi
                               else                      extraLibDirs bi,
    IPI.hsLibraries        = [ "HS" ++ display (packageId pkg) | hasLibrary ],
    IPI.extraLibraries     = extraLibs bi,
    IPI.extraGHCiLibraries = [],
    IPI.includeDirs        = absinc ++ adjustRelIncDirs relinc,
    IPI.includes           = includes bi,
    IPI.depends            = map fst (componentPackageDeps clbi),
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
        _ <- tryIO $ removeDirectoryRecursive (libdir installDirs)
        return ()
    NHC -> do
        _ <- tryIO $ removeDirectoryRecursive (libdir installDirs)
        return ()
    _ ->
        die ("only unregistering with GHC and Hugs is implemented")

unregScriptFileName :: FilePath
unregScriptFileName = case buildOS of
                          Windows -> "unregister.bat"
                          _       -> "unregister.sh"

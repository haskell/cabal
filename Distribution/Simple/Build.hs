-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point to actually building the modules in a package. It
-- doesn't actually do much itself, most of the work is delegated to
-- compiler-specific actions. It does do some non-compiler specific bits like
-- running pre-processors.
--

{- Copyright (c) 2003-2005, Isaac Jones
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

module Distribution.Simple.Build (
    build,

    initialBuildSteps,
    writeAutogenFiles,
  ) where

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.LHC  as LHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs
import qualified Distribution.Simple.UHC  as UHC

import qualified Distribution.Simple.Build.Macros      as Build.Macros
import qualified Distribution.Simple.Build.PathsModule as Build.PathsModule

import Distribution.Package
         ( Package(..) )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor, PackageDB(..) )
import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), Library(..), Executable(..)
         , TestSuite(..), TestType(..) )
import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.ModuleName as ModuleName

import Distribution.Simple.Setup
         ( BuildFlags(..), fromFlag )
import Distribution.Simple.PreProcess
         ( preprocessSources, PPSuffixHandler )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(compiler, buildDir, withPackageDB)
         , ComponentLocalBuildInfo, withLibLBI, withExeLBI
         , inplacePackageId, withTestLBI )
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, autogenModuleName, cppHeaderName )
import Distribution.Simple.Register
         ( registerPackage, inplaceInstalledPackageInfo )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, rewriteFile
         , die, info, setupMessage )

import Distribution.Version ( Version(..), withinRange, withinVersion )
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display, disp )

import Data.Maybe
         ( maybeToList )
import Control.Monad
         ( unless )
import System.FilePath
         ( (</>), (<.>) )
import System.Directory
         ( getCurrentDirectory )

-- -----------------------------------------------------------------------------
-- |Build the libraries and executables in this package.

build    :: PackageDescription  -- ^mostly information from the .cabal file
         -> LocalBuildInfo -- ^Configuration information
         -> BuildFlags -- ^Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = do
  let distPref  = fromFlag (buildDistPref flags)
      verbosity = fromFlag (buildVerbosity flags)
  initialBuildSteps distPref pkg_descr lbi verbosity suffixes
  setupMessage verbosity "Building" (packageId pkg_descr)

  internalPackageDB <- createInternalPackageDB distPref

  withLibLBI pkg_descr lbi $ \lib clbi -> do
    info verbosity "Building library..."
    buildLib verbosity pkg_descr lbi lib clbi

    -- Register the library in-place, so exes can depend
    -- on internally defined libraries.
    pwd <- getCurrentDirectory
    let installedPkgInfo =
          (inplaceInstalledPackageInfo pwd distPref pkg_descr lib lbi clbi) {
            -- The inplace registration uses the "-inplace" suffix,
            -- not an ABI hash.
            IPI.installedPackageId = inplacePackageId (packageId installedPkgInfo)
          }
    registerPackage verbosity
      installedPkgInfo pkg_descr lbi True -- True meaning inplace
      (withPackageDB lbi ++ [internalPackageDB])

  -- Use the internal package db for the exes.
  let lbi' = lbi { withPackageDB = withPackageDB lbi ++ [internalPackageDB] }

  withExeLBI pkg_descr lbi' $ \exe clbi -> do
    info verbosity $ "Building executable " ++ exeName exe ++ "..."
    buildExe verbosity pkg_descr lbi' exe clbi

  withTestLBI pkg_descr lbi' $ \test clbi ->
    case testType test of
        ExeTest v f -> if withinRange v (withinVersion $ Version [1,0] [])
            then do
                let exe = Executable
                        { exeName = testName test
                        , modulePath = f
                        , buildInfo = testBuildInfo test
                        }
                info verbosity $ "Building test suite " ++ testName test ++ "..."
                buildExe verbosity pkg_descr lbi' exe clbi
            else die $ "No support for building test suite type: "
                    ++ show (disp $ testType test)
        _ -> die $ "No support for building test suite type: "
                ++ show (disp $ testType test)


-- | Initialize a new package db file for libraries defined
-- internally to the package.
createInternalPackageDB :: FilePath -> IO PackageDB
createInternalPackageDB distPref = do
    let dbFile = distPref </> "package.conf.inplace"
        packageDB = SpecificPackageDB dbFile
    writeFile dbFile "[]"
    return packageDB

buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.buildLib  verbosity pkg_descr lbi lib clbi
    JHC  -> JHC.buildLib  verbosity pkg_descr lbi lib clbi
    LHC  -> LHC.buildLib  verbosity pkg_descr lbi lib clbi
    Hugs -> Hugs.buildLib verbosity pkg_descr lbi lib clbi
    NHC  -> NHC.buildLib  verbosity pkg_descr lbi lib clbi
    UHC  -> UHC.buildLib  verbosity pkg_descr lbi lib clbi
    _    -> die "Building is not supported with this compiler."

buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.buildExe  verbosity pkg_descr lbi exe clbi
    JHC  -> JHC.buildExe  verbosity pkg_descr lbi exe clbi
    LHC  -> LHC.buildExe  verbosity pkg_descr lbi exe clbi
    Hugs -> Hugs.buildExe verbosity pkg_descr lbi exe clbi
    NHC  -> NHC.buildExe  verbosity pkg_descr lbi exe clbi
    _    -> die "Building is not supported with this compiler."

initialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> Verbosity -- ^The verbosity to use
                  -> [ PPSuffixHandler ] -- ^preprocessors to run before compiling
                  -> IO ()
initialBuildSteps _distPref pkg_descr lbi verbosity suffixes = do
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = display (packageId pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)

  writeAutogenFiles verbosity pkg_descr lbi

  preprocessSources pkg_descr lbi False verbosity suffixes

-- | Generate and write out the Paths_<pkg>.hs and cabal_macros.h files
--
writeAutogenFiles :: Verbosity
                  -> PackageDescription
                  -> LocalBuildInfo
                  -> IO ()
writeAutogenFiles verbosity pkg lbi = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi)

  let pathsModulePath = autogenModulesDir lbi
                    </> ModuleName.toFilePath (autogenModuleName pkg) <.> "hs"
  rewriteFile pathsModulePath (Build.PathsModule.generate pkg lbi)

  let cppHeaderPath = autogenModulesDir lbi </> cppHeaderName
  rewriteFile cppHeaderPath (Build.Macros.generate pkg lbi)

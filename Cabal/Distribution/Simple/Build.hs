-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008, 2012
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
         ( Package(..), PackageName(..), PackageIdentifier(..)
         , Dependency(..), thisPackageVersion )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor, PackageDB(..) )
import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), Library(..), Executable(..)
         , TestSuite(..), TestSuiteInterface(..), Benchmark(..)
         , BenchmarkInterface(..) )
import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.ModuleName as ModuleName
import Distribution.ModuleName (ModuleName)

import Distribution.Simple.Setup
         ( BuildFlags(..), fromFlag )
import Distribution.Simple.BuildTarget
         ( BuildTarget(..), readBuildTargets )
import Distribution.Simple.PreProcess
         ( preprocessComponent, PPSuffixHandler )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(compiler, buildDir, withPackageDB, withPrograms)
         , Component(..), componentName, getComponent, componentBuildInfo
         , ComponentLocalBuildInfo(..), pkgEnabledComponents
         , withComponentsInBuildOrder, componentsInBuildOrder
         , ComponentName(..), showComponentName
         , ComponentDisabledReason(..), componentDisabledReason
         , inplacePackageId, LibraryName(..) )
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, autogenModuleName, cppHeaderName, exeExtension )
import Distribution.Simple.Register
         ( registerPackage, inplaceInstalledPackageInfo )
import Distribution.Simple.Test ( stubFilePath, stubName )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, rewriteFile
         , die, info, warn, setupMessage )

import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )

import Data.Maybe
         ( maybeToList )
import Data.Either
         ( partitionEithers )
import Data.List
         ( intersect, intercalate )
import Control.Monad
         ( when, unless, forM_ )
import System.FilePath
         ( (</>), (<.>) )
import System.Directory
         ( getCurrentDirectory )

-- -----------------------------------------------------------------------------
-- |Build the libraries and executables in this package.

build    :: PackageDescription  -- ^ Mostly information from the .cabal file
         -> LocalBuildInfo      -- ^ Configuration information
         -> BuildFlags          -- ^ Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^ preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = do
  let distPref  = fromFlag (buildDistPref flags)
      verbosity = fromFlag (buildVerbosity flags)

  targets  <- readBuildTargets pkg_descr (buildArgs flags)
  targets' <- checkBuildTargets verbosity pkg_descr targets
  let componentsToBuild = map fst (componentsInBuildOrder lbi (map fst targets'))
  info verbosity $ "Component build order: "
                ++ intercalate ", " (map showComponentName componentsToBuild)

  initialBuildSteps distPref pkg_descr lbi verbosity
  when (null targets) $
    -- Only bother with this message if we're building the whole package
    setupMessage verbosity "Building" (packageId pkg_descr)

  internalPackageDB <- createInternalPackageDB distPref

  withComponentsInBuildOrder pkg_descr lbi componentsToBuild $ \comp clbi ->
    let bi     = componentBuildInfo comp
        progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
        lbi'   = lbi {
                   withPrograms  = progs',
                   withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                 }
    in buildComponent verbosity pkg_descr lbi' suffixes comp clbi distPref


buildComponent :: Verbosity
               -> PackageDescription
               -> LocalBuildInfo
               -> [PPSuffixHandler]
               -> Component
               -> ComponentLocalBuildInfo
               -> FilePath
               -> IO ()
buildComponent verbosity pkg_descr lbi suffixes
               comp@(CLib lib) clbi distPref = do
    preprocessComponent pkg_descr comp lbi False verbosity suffixes
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
      (withPackageDB lbi)


buildComponent verbosity pkg_descr lbi suffixes
               comp@(CExe exe) clbi _ = do
    preprocessComponent pkg_descr comp lbi False verbosity suffixes
    info verbosity $ "Building executable " ++ exeName exe ++ "..."
    buildExe verbosity pkg_descr lbi exe clbi


buildComponent verbosity pkg_descr lbi suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteExeV10 _ f })
               clbi _distPref = do
    let bi  = testBuildInfo test
        exe = Executable {
                exeName    = testName test,
                modulePath = f,
                buildInfo  = bi
              }
    preprocessComponent pkg_descr comp lbi False verbosity suffixes
    info verbosity $ "Building test suite " ++ testName test ++ "..."
    buildExe verbosity pkg_descr lbi exe clbi


buildComponent verbosity pkg_descr lbi suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteLibV09 _ m })
               clbi -- This ComponentLocalBuildInfo corresponds to a detailed
                    -- test suite and not a real component. It should not
                    -- be used, except to construct the CLBIs for the
                    -- library and stub executable that will actually be
                    -- built.
               distPref = do
    pwd <- getCurrentDirectory
    let bi  = testBuildInfo test
        lib = Library {
                exposedModules = [ m ],
                libExposed     = True,
                libBuildInfo   = bi
              }
        libClbi = LibComponentLocalBuildInfo
                    { componentPackageDeps = componentPackageDeps clbi
                    , componentLibraries = [LibraryName (testName test)]
                    }
        pkg = pkg_descr {
                package      = (package pkg_descr) {
                                 pkgName = PackageName (testName test)
                               }
              , buildDepends = targetBuildDepends $ testBuildInfo test
              , executables  = []
              , testSuites   = []
              , library      = Just lib
              }
        ipi = (inplaceInstalledPackageInfo pwd distPref pkg lib lbi libClbi) {
                IPI.installedPackageId = inplacePackageId $ packageId ipi
              }
        testDir = buildDir lbi </> stubName test
              </> stubName test ++ "-tmp"
        testLibDep = thisPackageVersion $ package pkg
        exe = Executable {
                exeName    = stubName test,
                modulePath = stubFilePath test,
                buildInfo  = (testBuildInfo test) {
                               hsSourceDirs       = [ testDir ],
                               targetBuildDepends = testLibDep
                                 : (targetBuildDepends $ testBuildInfo test)
                             }
              }
        -- | The stub executable needs a new 'ComponentLocalBuildInfo'
        -- that exposes the relevant test suite library.
        exeClbi = ExeComponentLocalBuildInfo {
                    componentPackageDeps =
                        (IPI.installedPackageId ipi, packageId ipi)
                      : (filter (\(_, x) -> let PackageName name = pkgName x
                                            in name == "Cabal" || name == "base")
                                (componentPackageDeps clbi))
                  }
    preprocessComponent pkg_descr comp lbi False verbosity suffixes
    info verbosity $ "Building test suite " ++ testName test ++ "..."
    buildLib verbosity pkg lbi lib libClbi
    registerPackage verbosity ipi pkg lbi True $ withPackageDB lbi
    buildExe verbosity pkg_descr lbi exe exeClbi


buildComponent _ _ _ _
               (CTest TestSuite { testInterface = TestSuiteUnsupported tt })
               _ _ =
    die $ "No support for building test suite type " ++ display tt


buildComponent verbosity pkg_descr lbi suffixes
               comp@(CBench
                 bm@Benchmark { benchmarkInterface = BenchmarkExeV10 _ f })
               clbi _ = do
    let bi  = benchmarkBuildInfo bm
        exe = Executable
            { exeName = benchmarkName bm
            , modulePath = f
            , buildInfo  = bi
            }
        exeClbi = ExeComponentLocalBuildInfo
            { componentPackageDeps = componentPackageDeps clbi }
    preprocessComponent pkg_descr comp lbi False verbosity suffixes
    info verbosity $ "Building benchmark " ++ benchmarkName bm ++ "..."
    buildExe verbosity pkg_descr lbi exe exeClbi


buildComponent _ _ _ _
               (CBench Benchmark { benchmarkInterface = BenchmarkUnsupported tt })
               _ _ =
    die $ "No support for building benchmark type " ++ display tt


-- | Initialize a new package db file for libraries defined
-- internally to the package.
createInternalPackageDB :: FilePath -> IO PackageDB
createInternalPackageDB distPref = do
    let dbFile = distPref </> "package.conf.inplace"
        packageDB = SpecificPackageDB dbFile
    writeFile dbFile "[]"
    return packageDB

addInternalBuildTools :: PackageDescription -> LocalBuildInfo -> BuildInfo
                      -> ProgramDb -> ProgramDb
addInternalBuildTools pkg lbi bi progs =
    foldr updateProgram progs internalBuildTools
  where
    internalBuildTools =
      [ simpleConfiguredProgram toolName (FoundOnSystem toolLocation)
      | toolName <- toolNames
      , let toolLocation = buildDir lbi </> toolName </> toolName <.> exeExtension ]
    toolNames = intersect buildToolNames internalExeNames
    internalExeNames = map exeName (executables pkg)
    buildToolNames   = map buildToolName (buildTools bi)
      where
        buildToolName (Dependency (PackageName name) _ ) = name


-- TODO: build separate libs in separate dirs so that we can build
-- multiple libs, e.g. for 'LibTest' library-style testsuites
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
    UHC  -> UHC.buildExe  verbosity pkg_descr lbi exe clbi
    _    -> die "Building is not supported with this compiler."

initialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> Verbosity -- ^The verbosity to use
                  -> IO ()
initialBuildSteps _distPref pkg_descr lbi verbosity = do
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = display (packageId pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)

  writeAutogenFiles verbosity pkg_descr lbi

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

-- | Check that the given build targets are valid in the current context.
--
-- Also swizzle into a more convenient form.
--
checkBuildTargets :: Verbosity -> PackageDescription -> [BuildTarget]
                  -> IO [(ComponentName, Maybe (Either ModuleName FilePath))]
checkBuildTargets _ pkg []      =
    return [ (componentName c, Nothing) | c <- pkgEnabledComponents pkg ]

checkBuildTargets verbosity pkg targets = do

    let (enabled, disabled) =
          partitionEithers
            [ case componentDisabledReason (getComponent pkg cname) of
                Nothing     -> Left  target'
                Just reason -> Right (cname, reason)
            | target <- targets
            , let target'@(cname,_) = swizzleTarget target ]

    case disabled of
      []                 -> return ()
      ((cname,reason):_) -> die $ formatReason (showComponentName cname) reason

    forM_ [ (c, t) | (c, Just t) <- enabled ] $ \(c, t) ->
      warn verbosity $ "Ignoring '" ++ either display id t ++ ". The whole "
                    ++ showComponentName c ++ " will be built. (Support for "
                    ++ "module and file targets has not been implemented yet.)"

    return enabled

  where
    swizzleTarget (BuildTargetComponent c)   = (c, Nothing)
    swizzleTarget (BuildTargetModule    c m) = (c, Just (Left  m))
    swizzleTarget (BuildTargetFile      c f) = (c, Just (Right f))

    formatReason cn DisabledComponent =
        "Cannot build the " ++ cn ++ " because the component is marked "
     ++ "as disabled in the .cabal file."
    formatReason cn DisabledAllTests =
        "Cannot build the " ++ cn ++ " because test suites are not "
     ++ "enabled. Run configure with the flag --enable-tests"
    formatReason cn DisabledAllBenchmarks =
        "Cannot build the " ++ cn ++ " because benchmarks are not "
     ++ "enabled. Re-run configure with the flag --enable-benchmarks"

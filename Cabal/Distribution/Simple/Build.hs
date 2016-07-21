-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Build
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008, 2012
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point to actually building the modules in a package. It
-- doesn't actually do much itself, most of the work is delegated to
-- compiler-specific actions. It does do some non-compiler specific bits like
-- running pre-processors.
--

module Distribution.Simple.Build (
    build, repl,
    startInterpreter,

    initialBuildSteps,
    writeAutogenFiles,
  ) where

import Distribution.Package
import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite

import qualified Distribution.Simple.Build.Macros      as Build.Macros
import qualified Distribution.Simple.Build.PathsModule as Build.PathsModule
import qualified Distribution.Simple.Program.HcPkg as HcPkg

import Distribution.Simple.Compiler hiding (Flag)
import Distribution.PackageDescription hiding (Flag)
import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.ModuleName as ModuleName

import Distribution.Simple.Setup
import Distribution.Simple.BuildTarget
import Distribution.Simple.PreProcess
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.BuildPaths
import Distribution.Simple.Configure
import Distribution.Simple.Register
import Distribution.Simple.Test.LibV09
import Distribution.Simple.Utils

import Distribution.System
import Distribution.Text
import Distribution.Verbosity

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
         ( intersect )
import Control.Monad
         ( when, unless )
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
build pkg_descr lbi flags suffixes
 | fromFlag (buildAssumeDepsUpToDate flags) = do
  -- TODO: if checkBuildTargets ignores a target we may accept
  -- a --assume-deps-up-to-date with multiple arguments. Arguably, we should
  -- error early in this case.
  targets <- readBuildTargets pkg_descr (buildArgs flags)
  (cname, _) <- checkBuildTargets verbosity pkg_descr lbi targets >>= \r -> case r of
              [] -> die "In --assume-deps-up-to-date mode you must specify a target"
              [target'] -> return target'
              _ -> die "In --assume-deps-up-to-date mode you can only build a single target"
  -- NB: do NOT 'createInternalPackageDB'; we don't want to delete it.
  -- But this means we have to be careful about unregistering
  -- ourselves.
  let dbPath = internalPackageDBPath lbi distPref
      internalPackageDB = SpecificPackageDB dbPath
      clbi = getComponentLocalBuildInfo lbi cname
      comp = getComponent pkg_descr cname
  -- TODO: do we need to unregister libraries?  In any case, this would
  -- need to be done in the buildLib functionality.
  -- Do the build
  initialBuildSteps distPref pkg_descr lbi clbi verbosity
  let bi     = componentBuildInfo comp
      progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
      lbi'   = lbi {
                 withPrograms  = progs',
                 withPackageDB = withPackageDB lbi ++ [internalPackageDB]
               }
  buildComponent verbosity (buildNumJobs flags) pkg_descr
                 lbi' suffixes comp clbi distPref
 | otherwise = do
  targets  <- readBuildTargets pkg_descr (buildArgs flags)
  targets' <- checkBuildTargets verbosity pkg_descr lbi targets
  let componentsToBuild = componentsInBuildOrder lbi (map fst targets')
  info verbosity $ "Component build order: "
                ++ intercalate ", " (map (showComponentName . componentLocalName) componentsToBuild)

  when (null targets) $
    -- Only bother with this message if we're building the whole package
    setupMessage verbosity "Building" (packageId pkg_descr)

  internalPackageDB <- createInternalPackageDB verbosity lbi distPref

  -- TODO: we're computing this twice, do it once!
  withComponentsInBuildOrder pkg_descr lbi (map fst targets') $ \comp clbi -> do
    initialBuildSteps distPref pkg_descr lbi clbi verbosity
    let bi     = componentBuildInfo comp
        progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
        lbi'   = lbi {
                   withPrograms  = progs',
                   withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                 }
    buildComponent verbosity (buildNumJobs flags) pkg_descr
                   lbi' suffixes comp clbi distPref
 where
  distPref  = fromFlag (buildDistPref flags)
  verbosity = fromFlag (buildVerbosity flags)


repl     :: PackageDescription  -- ^ Mostly information from the .cabal file
         -> LocalBuildInfo      -- ^ Configuration information
         -> ReplFlags           -- ^ Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^ preprocessors to run before compiling
         -> [String]
         -> IO ()
repl pkg_descr lbi flags suffixes args = do
  let distPref  = fromFlag (replDistPref flags)
      verbosity = fromFlag (replVerbosity flags)

  targets  <- readBuildTargets pkg_descr args
  targets' <- case targets of
    -- This seems DEEPLY questionable.
    []       -> return $ take 1 [ componentName c
                                | c <- pkgBuildableComponents pkg_descr ]
    [target] -> fmap (map fst) (checkBuildTargets verbosity pkg_descr lbi [target])
    _        -> die $ "The 'repl' command does not support multiple targets at once."
  let componentsToBuild = componentsInBuildOrder lbi targets'
      componentForRepl  = last componentsToBuild
  debug verbosity $ "Component build order: "
                 ++ intercalate ", "
                      [ showComponentName (componentLocalName clbi) | clbi <- componentsToBuild ]

  internalPackageDB <- createInternalPackageDB verbosity lbi distPref

  let lbiForComponent comp lbi' =
        lbi' {
          withPackageDB = withPackageDB lbi ++ [internalPackageDB],
          withPrograms  = addInternalBuildTools pkg_descr lbi'
                            (componentBuildInfo comp) (withPrograms lbi')
        }

  -- build any dependent components
  sequence_
    [ do let cname = componentLocalName clbi
             comp = getComponent pkg_descr cname
             lbi' = lbiForComponent comp lbi
         initialBuildSteps distPref pkg_descr lbi clbi verbosity
         buildComponent verbosity NoFlag
                        pkg_descr lbi' suffixes comp clbi distPref
    | clbi <- init componentsToBuild ]

  -- REPL for target components
  let clbi = componentForRepl
      cname = componentLocalName clbi
      comp = getComponent pkg_descr cname
      lbi' = lbiForComponent comp lbi
  initialBuildSteps distPref pkg_descr lbi clbi verbosity
  replComponent verbosity pkg_descr lbi' suffixes comp clbi distPref


-- | Start an interpreter without loading any package files.
startInterpreter :: Verbosity -> ProgramDb -> Compiler -> Platform
                 -> PackageDBStack -> IO ()
startInterpreter verbosity programDb comp platform packageDBs =
  case compilerFlavor comp of
    GHC   -> GHC.startInterpreter   verbosity programDb comp platform packageDBs
    GHCJS -> GHCJS.startInterpreter verbosity programDb comp platform packageDBs
    _     -> die "A REPL is not supported with this compiler."

buildComponent :: Verbosity
               -> Flag (Maybe Int)
               -> PackageDescription
               -> LocalBuildInfo
               -> [PPSuffixHandler]
               -> Component
               -> ComponentLocalBuildInfo
               -> FilePath
               -> IO ()
buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CLib lib) clbi distPref = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    case libName lib of
        Nothing -> info verbosity $ "Building library..."
        Just n -> info verbosity $ "Building library " ++ n ++ "..."
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = addExtraCSources libbi extras }
    buildLib verbosity numJobs pkg_descr lbi lib' clbi

    -- Register the library in-place, so exes can depend
    -- on internally defined libraries.
    pwd <- getCurrentDirectory
    let -- The in place registration uses the "-inplace" suffix, not an ABI hash
        installedPkgInfo = inplaceInstalledPackageInfo pwd distPref pkg_descr
                                                       (AbiHash "") lib' lbi clbi

    registerPackage verbosity (compiler lbi) (withPrograms lbi) HcPkg.MultiInstance
                    (withPackageDB lbi) installedPkgInfo

buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CExe exe) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    info verbosity $ "Building executable " ++ exeName exe ++ "..."
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' clbi


buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CTest test@TestSuite { testInterface = TestSuiteExeV10{} })
               clbi _distPref = do
    let exe = testSuiteExeV10AsExe test
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    info verbosity $ "Building test suite " ++ testName test ++ "..."
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' clbi


buildComponent verbosity numJobs pkg_descr lbi0 suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteLibV09{} })
               clbi -- This ComponentLocalBuildInfo corresponds to a detailed
                    -- test suite and not a real component. It should not
                    -- be used, except to construct the CLBIs for the
                    -- library and stub executable that will actually be
                    -- built.
               distPref = do
    pwd <- getCurrentDirectory
    let (pkg, lib, libClbi, lbi, ipi, exe, exeClbi) =
          testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 distPref pwd
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    info verbosity $ "Building test suite " ++ testName test ++ "..."
    buildLib verbosity numJobs pkg lbi lib libClbi
    -- NB: need to enable multiple instances here, because on 7.10+
    -- the package name is the same as the library, and we still
    -- want the registration to go through.
    registerPackage verbosity (compiler lbi) (withPrograms lbi) HcPkg.MultiInstance
                    (withPackageDB lbi) ipi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' exeClbi


buildComponent _ _ _ _ _
               (CTest TestSuite { testInterface = TestSuiteUnsupported tt })
               _ _ =
    die $ "No support for building test suite type " ++ display tt


buildComponent verbosity numJobs pkg_descr lbi suffixes
               comp@(CBench bm@Benchmark { benchmarkInterface = BenchmarkExeV10 {} })
               clbi _ = do
    let (exe, exeClbi) = benchmarkExeV10asExe bm clbi
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    info verbosity $ "Building benchmark " ++ benchmarkName bm ++ "..."
    let ebi = buildInfo exe
        exe' = exe { buildInfo = addExtraCSources ebi extras }
    buildExe verbosity numJobs pkg_descr lbi exe' exeClbi


buildComponent _ _ _ _ _
               (CBench Benchmark { benchmarkInterface = BenchmarkUnsupported tt })
               _ _ =
    die $ "No support for building benchmark type " ++ display tt


-- | Add extra C sources generated by preprocessing to build
-- information.
addExtraCSources :: BuildInfo -> [FilePath] -> BuildInfo
addExtraCSources bi extras = bi { cSources = new }
  where new = Set.toList $ old `Set.union` exs
        old = Set.fromList $ cSources bi
        exs = Set.fromList extras


replComponent :: Verbosity
              -> PackageDescription
              -> LocalBuildInfo
              -> [PPSuffixHandler]
              -> Component
              -> ComponentLocalBuildInfo
              -> FilePath
              -> IO ()
replComponent verbosity pkg_descr lbi suffixes
               comp@(CLib lib) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = libbi { cSources = cSources libbi ++ extras } }
    replLib verbosity pkg_descr lbi lib' clbi

replComponent verbosity pkg_descr lbi suffixes
               comp@(CExe exe) clbi _ = do
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = ebi { cSources = cSources ebi ++ extras } }
    replExe verbosity pkg_descr lbi exe' clbi


replComponent verbosity pkg_descr lbi suffixes
               comp@(CTest test@TestSuite { testInterface = TestSuiteExeV10{} })
               clbi _distPref = do
    let exe = testSuiteExeV10AsExe test
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = ebi { cSources = cSources ebi ++ extras } }
    replExe verbosity pkg_descr lbi exe' clbi


replComponent verbosity pkg_descr lbi0 suffixes
               comp@(CTest
                 test@TestSuite { testInterface = TestSuiteLibV09{} })
               clbi distPref = do
    pwd <- getCurrentDirectory
    let (pkg, lib, libClbi, lbi, _, _, _) =
          testSuiteLibV09AsLibAndExe pkg_descr test clbi lbi0 distPref pwd
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    let libbi = libBuildInfo lib
        lib' = lib { libBuildInfo = libbi { cSources = cSources libbi ++ extras } }
    replLib verbosity pkg lbi lib' libClbi


replComponent _ _ _ _
              (CTest TestSuite { testInterface = TestSuiteUnsupported tt })
              _ _ =
    die $ "No support for building test suite type " ++ display tt


replComponent verbosity pkg_descr lbi suffixes
               comp@(CBench bm@Benchmark { benchmarkInterface = BenchmarkExeV10 {} })
               clbi _ = do
    let (exe, exeClbi) = benchmarkExeV10asExe bm clbi
    preprocessComponent pkg_descr comp lbi clbi False verbosity suffixes
    extras <- preprocessExtras comp lbi
    let ebi = buildInfo exe
        exe' = exe { buildInfo = ebi { cSources = cSources ebi ++ extras } }
    replExe verbosity pkg_descr lbi exe' exeClbi


replComponent _ _ _ _
              (CBench Benchmark { benchmarkInterface = BenchmarkUnsupported tt })
              _ _ =
    die $ "No support for building benchmark type " ++ display tt

----------------------------------------------------
-- Shared code for buildComponent and replComponent
--

-- | Translate a exe-style 'TestSuite' component into an exe for building
testSuiteExeV10AsExe :: TestSuite -> Executable
testSuiteExeV10AsExe test@TestSuite { testInterface = TestSuiteExeV10 _ mainFile } =
    Executable {
      exeName    = testName test,
      modulePath = mainFile,
      buildInfo  = testBuildInfo test
    }
testSuiteExeV10AsExe TestSuite{} = error "testSuiteExeV10AsExe: wrong kind"

-- | Translate a lib-style 'TestSuite' component into a lib + exe for building
testSuiteLibV09AsLibAndExe :: PackageDescription
                           -> TestSuite
                           -> ComponentLocalBuildInfo
                           -> LocalBuildInfo
                           -> FilePath
                           -> FilePath
                           -> (PackageDescription,
                               Library, ComponentLocalBuildInfo,
                               LocalBuildInfo,
                               IPI.InstalledPackageInfo,
                               Executable, ComponentLocalBuildInfo)
testSuiteLibV09AsLibAndExe pkg_descr
                     test@TestSuite { testInterface = TestSuiteLibV09 _ m }
                     clbi lbi distPref pwd =
    (pkg, lib, libClbi, lbi, ipi, exe, exeClbi)
  where
    bi  = testBuildInfo test
    lib = Library {
            libName = Nothing,
            exposedModules = [ m ],
            reexportedModules = [],
            requiredSignatures = [],
            libExposed     = True,
            libBuildInfo   = bi
          }
    -- This is, like, the one place where we use a CTestName for a library.
    -- Should NOT use library name, since that could conflict!
    PackageIdentifier pkg_name pkg_ver = package pkg_descr
    compat_name = computeCompatPackageName pkg_name (CTestName (testName test))
    compat_key = computeCompatPackageKey (compiler lbi) compat_name pkg_ver (componentUnitId clbi)
    libClbi = LibComponentLocalBuildInfo
                { componentPackageDeps = componentPackageDeps clbi
                , componentLocalName = CSubLibName (testName test)
                , componentIsPublic = False
                , componentIncludes = componentIncludes clbi
                , componentUnitId = componentUnitId clbi
                , componentCompatPackageName = compat_name
                , componentCompatPackageKey = compat_key
                , componentExposedModules = [IPI.ExposedModule m Nothing]
                }
    pkg = pkg_descr {
            package      = (package pkg_descr) { pkgName = compat_name }
          , buildDepends = targetBuildDepends $ testBuildInfo test
          , executables  = []
          , testSuites   = []
          , subLibraries = [lib]
          }
    ipi    = inplaceInstalledPackageInfo pwd distPref pkg (AbiHash "") lib lbi libClbi
    testDir = buildDir lbi </> stubName test
          </> stubName test ++ "-tmp"
    testLibDep = thisPackageVersion $ package pkg
    exe = Executable {
            exeName    = stubName test,
            modulePath = stubFilePath test,
            buildInfo  = (testBuildInfo test) {
                           hsSourceDirs       = [ testDir ],
                           targetBuildDepends = testLibDep
                             : (targetBuildDepends $ testBuildInfo test),
                           targetBuildRenaming = Map.empty
                         }
          }
    -- | The stub executable needs a new 'ComponentLocalBuildInfo'
    -- that exposes the relevant test suite library.
    deps = (IPI.installedUnitId ipi, packageId ipi)
         : (filter (\(_, x) -> let PackageName name = pkgName x
                               in name == "Cabal" || name == "base")
                   (componentPackageDeps clbi))
    exeClbi = ExeComponentLocalBuildInfo {
                -- TODO: this is a hack, but as long as this is unique
                -- (doesn't clobber something) we won't run into trouble
                componentUnitId = mkUnitId (stubName test),
                componentLocalName = CExeName (stubName test),
                componentPackageDeps = deps,
                componentIncludes = zip (map fst deps) (repeat defaultRenaming)
              }
testSuiteLibV09AsLibAndExe _ TestSuite{} _ _ _ _ = error "testSuiteLibV09AsLibAndExe: wrong kind"


-- | Translate a exe-style 'Benchmark' component into an exe for building
benchmarkExeV10asExe :: Benchmark -> ComponentLocalBuildInfo
                     -> (Executable, ComponentLocalBuildInfo)
benchmarkExeV10asExe bm@Benchmark { benchmarkInterface = BenchmarkExeV10 _ f }
                     clbi =
    (exe, exeClbi)
  where
    exe = Executable {
            exeName    = benchmarkName bm,
            modulePath = f,
            buildInfo  = benchmarkBuildInfo bm
          }
    exeClbi = ExeComponentLocalBuildInfo {
                componentUnitId = componentUnitId clbi,
                componentLocalName = CExeName (benchmarkName bm),
                componentPackageDeps = componentPackageDeps clbi,
                componentIncludes = componentIncludes clbi
              }
benchmarkExeV10asExe Benchmark{} _ = error "benchmarkExeV10asExe: wrong kind"

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
-- multiple libs, e.g. for 'LibTest' library-style test suites
buildLib :: Verbosity -> Flag (Maybe Int)
                      -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity numJobs pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.buildLib   verbosity numJobs pkg_descr lbi lib clbi
    GHCJS -> GHCJS.buildLib verbosity numJobs pkg_descr lbi lib clbi
    JHC   -> JHC.buildLib   verbosity         pkg_descr lbi lib clbi
    LHC   -> LHC.buildLib   verbosity         pkg_descr lbi lib clbi
    UHC   -> UHC.buildLib   verbosity         pkg_descr lbi lib clbi
    HaskellSuite {} -> HaskellSuite.buildLib verbosity pkg_descr lbi lib clbi
    _    -> die "Building is not supported with this compiler."

buildExe :: Verbosity -> Flag (Maybe Int)
                      -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity numJobs pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.buildExe   verbosity numJobs pkg_descr lbi exe clbi
    GHCJS -> GHCJS.buildExe verbosity numJobs pkg_descr lbi exe clbi
    JHC   -> JHC.buildExe   verbosity         pkg_descr lbi exe clbi
    LHC   -> LHC.buildExe   verbosity         pkg_descr lbi exe clbi
    UHC   -> UHC.buildExe   verbosity         pkg_descr lbi exe clbi
    _     -> die "Building is not supported with this compiler."

replLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                     -> Library            -> ComponentLocalBuildInfo -> IO ()
replLib verbosity pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    -- 'cabal repl' doesn't need to support 'ghc --make -j', so we just pass
    -- NoFlag as the numJobs parameter.
    GHC   -> GHC.replLib   verbosity NoFlag pkg_descr lbi lib clbi
    GHCJS -> GHCJS.replLib verbosity NoFlag pkg_descr lbi lib clbi
    _     -> die "A REPL is not supported for this compiler."

replExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                     -> Executable         -> ComponentLocalBuildInfo -> IO ()
replExe verbosity pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC   -> GHC.replExe   verbosity NoFlag pkg_descr lbi exe clbi
    GHCJS -> GHCJS.replExe verbosity NoFlag pkg_descr lbi exe clbi
    _     -> die "A REPL is not supported for this compiler."


initialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .cabal file
                  -> LocalBuildInfo -- ^Configuration information
                  -> ComponentLocalBuildInfo
                  -> Verbosity -- ^The verbosity to use
                  -> IO ()
initialBuildSteps _distPref pkg_descr lbi clbi verbosity = do
  -- check that there's something to build
  unless (not . null $ allBuildInfo pkg_descr) $ do
    let name = display (packageId pkg_descr)
    die $ "No libraries, executables, tests, or benchmarks "
       ++ "are enabled for package " ++ name ++ "."

  createDirectoryIfMissingVerbose verbosity True (componentBuildDir lbi clbi)

  writeAutogenFiles verbosity pkg_descr lbi clbi

-- | Generate and write out the Paths_<pkg>.hs and cabal_macros.h files
--
writeAutogenFiles :: Verbosity
                  -> PackageDescription
                  -> LocalBuildInfo
                  -> ComponentLocalBuildInfo
                  -> IO ()
writeAutogenFiles verbosity pkg lbi clbi = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi clbi)

  let pathsModulePath = autogenModulesDir lbi clbi
                    </> ModuleName.toFilePath (autogenModuleName pkg) <.> "hs"
  rewriteFile pathsModulePath (Build.PathsModule.generate pkg lbi clbi)

  let cppHeaderPath = autogenModulesDir lbi clbi </> cppHeaderName
  rewriteFile cppHeaderPath (Build.Macros.generate pkg lbi clbi)

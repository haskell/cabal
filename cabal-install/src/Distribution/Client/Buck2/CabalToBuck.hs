-- | Turns one local package's already-resolved 'PackageDescription' (flags
-- and conditionals already flattened by the solver, so gated
-- @cxx-sources@\/@ghc-options@\/etc. from an @if flag(...)@ stanza show up
-- here exactly as they should for the resolved build) into the buck2 rule
-- calls for its @BUCK.cabal.bzl@ file: 'haskell_library' \/ 'haskell_binary'
-- \/ 'haskell_test' for each buildable component, plus a 'cxx_library' for
-- any component with @cxx-sources@\/@c-sources@ and an
-- @external_pkgconfig_library@ for each distinct @pkgconfig-depends@.
module Distribution.Client.Buck2.CabalToBuck
  ( LocalPackageIndex
  , PackageTargets (..)
  , generatePackageTargets
  , libTargetName
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Compiler (CompilerFlavor (GHC))
import qualified Distribution.ModuleName as ModuleName
import Distribution.Package (packageId, packageName)
import Distribution.PackageDescription
  ( Benchmark (benchmarkInterface, benchmarkName)
  , BenchmarkInterface (..)
  , BuildInfo
  , Executable (exeName, modulePath)
  , Library (exposedModules, libBuildInfo, libName)
  , LibraryName (..)
  , PackageDescription
  , TestSuite (testInterface, testName)
  , TestSuiteInterface (..)
  , buildToolDepends
  , cppOptions
  , cxxOptions
  , cxxSources
  , cSources
  , defaultExtensions
  , defaultLanguage
  , extraLibs
  , hcOptions
  , hsSourceDirs
  , includeDirs
  , otherModules
  , pkgBuildableComponents
  , pkgconfigDepends
  , targetBuildDepends
  )
import Distribution.Simple.Compiler (compilerInfo)
import Distribution.Types.Component (Component (..), componentBuildInfo, componentName)
import Distribution.Types.ComponentLocalBuildInfo (ComponentLocalBuildInfo)
import Distribution.Types.ComponentName (ComponentName)
import Distribution.Types.Dependency (depLibraries, depPkgName)
import Distribution.Types.ExeDependency (ExeDependency (..))
import Distribution.Types.LocalBuildInfo (LocalBuildInfo (compiler, hostPlatform, withPrograms), componentNameCLBIs, localUnitId)
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
import Distribution.Types.PkgconfigName (unPkgconfigName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity (VerbosityFlags (vLevel), VerbosityLevel (Silent), modifyVerbosityFlags)

import Distribution.Simple.Build.Macros (generateCabalMacrosHeader)
import Distribution.Simple.Build.PathsModule (generatePathsModule)
import Distribution.Simple.BuildPaths (autogenPathsModuleName)
import Distribution.Simple.InstallDirs
  ( PathTemplate
  , PathTemplateVariable (TestSuiteNameVar)
  , fromPathTemplate
  , initialPathTemplateEnv
  , substPathTemplate
  , toPathTemplate
  )
import Distribution.Simple.Program.Builtin (ghcProgram)
import Distribution.Simple.Program.Db (lookupProgram)
import Distribution.Simple.Program.Types (programOverrideArgs)
import Distribution.Simple.Utils (ordNub, warn)

import Distribution.Client.Buck2.Starlark

-- | Maps every local project package's name to the buck2 cell-relative
-- directory its @BUCK@ file lives in (@.@ for one at the project root), so
-- a dependency on another local package can be turned into a fully
-- qualified target label - plus the set of other packages its main
-- library's @reexported-modules@ re-export from (see 'classifyDeps').
type LocalPackageIndex = Map PackageName (FilePath, [PackageName])

-- | What is generated for one package: its build spec (see 'packageSpec'),
-- plus every generated autogen file (a component's @cabal_macros.h@, or the
-- package's own @Paths_\<pkg\>@ module) that needs an @export_file()@ entry
-- in @cabal-buck2\/autogen\/BUCK@ (see "Distribution.Client.Buck2.Generate")
-- so it can be referenced - by a component's @cabal_component@, or by a
-- hand-written rule elsewhere in the project - as a real, buck2-tracked
-- target instead of an untracked path string. Each entry is
-- @(exportTargetName, pathRelativeToCabalBuck2Autogen)@; 'writeMacrosHeader',
-- 'writePathsModule' and 'writeDetailedTestStub' are the only producers.
data PackageTargets = PackageTargets
  { ptSpec :: Value
  , ptComponentCount :: Int
  , ptAutogenExports :: [(String, FilePath)]
  }

-- | One component's contribution to a 'PackageTargets': its entry in the
-- spec, and its autogen exports.
type ComponentTargets = ([Value], [(String, FilePath)])

componentTargets :: Value -> [(String, FilePath)] -> ComponentTargets
componentTargets component exports = ([component], exports)

-- | Generate the buck2 targets for every buildable component of a local
-- package. @pkgDir@ is the package's own directory (where the generated
-- @BUCK.cabal.bzl@ will live) - every emitted path is relative to it.
generatePackageTargets
  :: Verbosity
  -> LocalPackageIndex
  -> FilePath
  -- ^ @pkgDir@'s own buck2 cell-relative directory (@.@ at the project
  -- root) - used only to locate the generated @cabal_macros.h@ from the
  -- compile action's working directory (the project root), distinct from
  -- @pkgDir@ itself (a real filesystem path, used for everything else).
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -- ^ Every @build-tool-depends:@ executable name
  -- "Distribution.Client.Buck2.Prebuilt" resolved a real *external*
  -- binary for (and generated an @export_file()@ target for) - see
  -- 'specComponent'.
  -> Map (PackageName, ComponentName) [PathTemplate]
  -- ^ The project's @test-options:@ for each test-suite (see 'testSuiteArgs').
  -> FilePath
  -> PackageDescription
  -> IO (Maybe PackageTargets)
generatePackageTargets verbosity localIndex rootRelPkgDir componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc = do
  -- Computed up front (silently - see 'skippedLibraries's own haddock),
  -- so every component below - regardless of its own textual position
  -- in the .cabal file relative to the library it depends on - already
  -- knows which of this package's own libraries won't get a rule, and
  -- can skip itself too instead of emitting a dangling dependency edge.
  skippedLibs <- skippedLibraries verbosity pkgDesc pkgDir componentLBIs
  (components, exports) <-
    mconcat
      <$> traverse
        (generateComponent verbosity localIndex componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc skippedLibs)
        (pkgBuildableComponents pkgDesc)
  return $
    if null components
      then Nothing
      else
        Just
          PackageTargets
            { ptSpec = packageSpec pkgDesc rootRelPkgDir (packageGhcOptions pkgDesc componentLBIs) components
            , ptComponentCount = length components
            , ptAutogenExports = ordNub exports
            }

-- | Which of this package's own libraries 'generateComponent' is going
-- to skip (unresolvable modules - see 'resolveModules'), found out
-- ahead of the real per-component pass so a *different* component that
-- depends on one (e.g. cabal-testsuite's own @test-runtime-deps@
-- executable, which build-depends on cabal-testsuite's own library) can
-- skip itself too, rather than emitting a rule whose @deps@ references a
-- target that was never generated - buck2 fails that outright at
-- analysis time ("Unknown target"), for the *entire* build, the same
-- class of problem 'resolveModules' itself exists to avoid for a single
-- component's own missing source file.
--
-- Runs with 'silent' verbosity deliberately: it duplicates exactly the
-- same resolution 'generateComponent' below will redo for real (and
-- warn about) when it reaches that library component itself - this
-- pass exists only to know the *outcome* early, not to report it twice.
skippedLibraries :: Verbosity -> PackageDescription -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> IO (Set LibraryName)
skippedLibraries verbosity pkgDesc pkgDir componentLBIs =
  Set.fromList . catMaybes
    <$> traverse checkLib [lib | CLib lib <- pkgBuildableComponents pkgDesc]
  where
    quiet = modifyVerbosityFlags (\vf -> vf{vLevel = Silent}) verbosity
    checkLib lib = do
      let bi = libBuildInfo lib
      msrcs <- resolveModules quiet pkgDesc (lbiClbiFor pkgDesc componentLBIs (CLib lib)) pkgDir bi (exposedModules lib ++ otherModules bi)
      return $ if isNothing msrcs then Just (libName lib) else Nothing

-- | Look up a component's real, Cabal-computed 'LocalBuildInfo' (from
-- "Distribution.Client.Buck2.Configure") plus its own 'ComponentLocalBuildInfo'
-- within it (via 'componentNameCLBIs') - 'Nothing' if either lookup fails
-- (e.g. a component that isn't part of the elaborated build plan, such as
-- a test-suite when tests aren't enabled), in which case callers skip the
-- component rather than emit a rule built from fabricated data.
lbiClbiFor :: PackageDescription -> Map (PackageName, ComponentName) LocalBuildInfo -> Component -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo)
lbiClbiFor pkgDesc componentLBIs comp = do
  lbi <- Map.lookup (packageName pkgDesc, componentName comp) componentLBIs
  clbi <- listToMaybe (componentNameCLBIs lbi (componentName comp))
  return (lbi, clbi)

generateComponent
  :: Verbosity
  -> LocalPackageIndex
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -> Map (PackageName, ComponentName) [PathTemplate]
  -> FilePath
  -> PackageDescription
  -> Set LibraryName
  -> Component
  -> IO ComponentTargets
generateComponent verbosity localIndex componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc skippedLibs comp = case comp of
  CLib lib -> library (libTargetName (packageName pkgDesc) (libName lib)) lib
  CExe exe -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (exeName exe)) "executable" $ executable exe
  CTest test -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (testName test)) "test-suite" $ testSuite test
  CBench bench -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (benchmarkName bench)) "benchmark" $ benchmark bench
  CFLib _ -> skip "foreign library (not supported yet)"
  where
    skip why = do
      warn verbosity $ "cabal buck2: skipping " ++ why ++ " in package " ++ unPackageName (packageName pkgDesc)
      return mempty

    -- A component that build-depends on one of *this same package's*
    -- own libraries, when that library was itself skipped (see
    -- 'skippedLibraries'), can't be built either - it would emit a rule
    -- whose own @deps@ references a target that was never generated,
    -- which buck2 rejects outright ("Unknown target") at analysis time
    -- for the whole build, not just a warning. cabal-testsuite's own
    -- @test-runtime-deps@ executable (build-depends on cabal-testsuite's
    -- own library) is exactly this case.
    ifNotOnSkippedLib bi targetName kind act =
      case [ln | d <- targetBuildDepends bi, depPkgName d == packageName pkgDesc, ln <- NES.toList (depLibraries d), ln `Set.member` skippedLibs] of
        (ln : _) ->
          skip
            ( kind
                ++ " "
                ++ targetName
                ++ " (depends on "
                ++ libTargetName (packageName pkgDesc) ln
                ++ ", itself skipped)"
            )
        [] -> act

    library targetName lib = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip ("library " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> do
        let bi = libBuildInfo lib
        msrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) pkgDir bi (exposedModules lib ++ otherModules bi)
        case msrcs of
          Nothing -> skip ("library " ++ targetName ++ " (couldn't resolve all its modules)")
          Just (srcs, srcAutogenExports) -> do
            macrosExport <- writeMacrosHeader pkgDir targetName pkgDesc lbi clbi
            return $
              componentTargets
                (specComponent localIndex externalBuildTools "library" targetName bi [("srcs", VDict [(m, srcSpec x) | (m, x) <- srcs])])
                (macrosExport : srcAutogenExports)

    -- An executable, benchmark or test-suite: all the same shape (a main
    -- module over some others), differing only in the spec's @kind@ and
    -- extras.
    executableLike kind targetName lbi clbi bi mainSrc otherSrcs extra srcAutogenExports = do
      macrosExport <- writeMacrosHeader pkgDir targetName pkgDesc lbi clbi
      return $
        componentTargets
          ( specComponent
              localIndex
              externalBuildTools
              kind
              targetName
              bi
              ([("main_is", srcSpec mainSrc)] ++ [("srcs", VDict [(m, srcSpec x) | (m, x) <- otherSrcs]) | not (null otherSrcs)] ++ extra)
          )
          (macrosExport : srcAutogenExports)

    executable exe = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip ("executable " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> do
        mmainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath (modulePath exe))
        motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) pkgDir bi (otherModules bi)
        case (mmainSrc, motherSrcs) of
          (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) ->
            executableLike "executable" targetName lbi clbi bi (SrcFile mainSrc0) otherSrcs [] srcAutogenExports
          _ -> skip ("executable " ++ targetName ++ " (couldn't resolve all its modules)")
      where
        bi = componentBuildInfo (CExe exe)
        targetName = unUnqualComponentName (exeName exe)

    -- A benchmark's own 'BenchmarkExeV10' is exactly 'TestSuiteExeV10's
    -- shape (a version-tagged main-is path over the same 'BuildInfo') -
    -- and unlike a test-suite, @cabal bench@ has no special "run it and
    -- report a testsuite-style result" semantics of its own, just
    -- "build and run this executable" - so this reuses 'executable's
    -- plain @haskell_binary()@ mapping verbatim, with no @cwd@ wrapper
    -- (matching how a plain executable is already generated here).
    benchmark bench = case benchmarkInterface bench of
      BenchmarkExeV10 _ver mainIs -> case lbiClbiFor pkgDesc componentLBIs comp of
        Nothing -> skip ("benchmark " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
        Just (lbi, clbi) -> do
          mmainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath mainIs)
          motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) pkgDir bi (otherModules bi)
          case (mmainSrc, motherSrcs) of
            (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) ->
              executableLike "benchmark" targetName lbi clbi bi (SrcFile mainSrc0) otherSrcs [] srcAutogenExports
            _ -> skip ("benchmark " ++ targetName ++ " (couldn't resolve all its modules)")
      _ ->
        skip
          ( "benchmark "
              ++ targetName
              ++ " (only exitcode-stdio-1.0 benchmarks are supported)"
          )
      where
        bi = componentBuildInfo (CBench bench)
        targetName = unUnqualComponentName (benchmarkName bench)

    testSuite test = case testInterface test of
      TestSuiteExeV10 _ver mainIs -> case lbiClbiFor pkgDesc componentLBIs comp of
        Nothing -> skip ("test-suite " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
        Just (lbi, clbi) -> do
          mmainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath mainIs)
          motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) pkgDir bi (otherModules bi)
          case (mmainSrc, motherSrcs) of
            (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) ->
              mkTestCall lbi clbi (SrcFile mainSrc0) otherSrcs srcAutogenExports
            _ -> skip ("test-suite " ++ targetName ++ " (couldn't resolve all its modules)")
      -- A @detailed-0.9@ test-suite's own module (named via
      -- @test-module:@, not @other-modules:@ - real Cabal synthesises a
      -- whole separate internal sub-library exposing just this one
      -- module, see 'Distribution.Simple.Build.testSuiteLibV09AsLibAndExe')
      -- exports @tests :: IO ['Distribution.TestSuite.Test']@, and real
      -- Cabal's own Setup.hs generates a tiny stub 'Main' importing it
      -- and calling into 'Distribution.Simple.Test.LibV09.stubMain' -
      -- which then blocks reading a @(logFilePath, testSuiteName)@ pair
      -- off *stdin*, written by the parent @cabal test@ process, before
      -- it'll run anything at all (see that module's own 'stubMain').
      -- That stdin handshake has nothing to do with buck2 - a
      -- @haskell_test()@ just execs the compiled binary and checks its
      -- exit code, the same as @exitcode-stdio-1.0@ - so reusing real
      -- Cabal's own stub verbatim would need a wrapper script to feed it
      -- a fake handshake for no real benefit (nothing here ever reads
      -- the machine-readable log it writes). Generates a self-contained
      -- stub instead, calling only 'Distribution.TestSuite's own public
      -- API directly (a real, documented, GHC-version-independent
      -- interface - not reimplementing anything real Cabal doesn't
      -- already expose for exactly this purpose): runs every 'Test'
      -- in turn, printing a human-readable pass\/fail\/error line per
      -- test to stdout, exiting non-zero if anything failed or errored.
      -- Needs no new build-depends beyond what real Cabal already
      -- requires the user to declare for their own @tests@ module to
      -- even type-check (@Distribution.TestSuite@ lives in the @Cabal@
      -- library itself).
      TestSuiteLibV09 _ver testModule -> case lbiClbiFor pkgDesc componentLBIs comp of
        Nothing -> skip ("test-suite " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
        Just (lbi, clbi) -> do
          mtestModSrc <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) pkgDir bi [testModule]
          motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) pkgDir bi (otherModules bi)
          case (mtestModSrc, motherSrcs) of
            (Just (testModSrc, testModAutogenExports), Just (otherSrcs, otherAutogenExports)) -> do
              (stubSrc, stubAutogenExport) <- writeDetailedTestStub pkgDir targetName testModule
              mkTestCall
                lbi
                clbi
                stubSrc
                (testModSrc ++ otherSrcs)
                (stubAutogenExport : testModAutogenExports ++ otherAutogenExports)
            _ -> skip ("test-suite " ++ targetName ++ " (couldn't resolve all its modules)")
      _ ->
        skip
          ( "test-suite "
              ++ targetName
              ++ " (only exitcode-stdio-1.0 and detailed-0.9 test-suites are supported)"
          )
      where
        bi = componentBuildInfo (CTest test)
        targetName = unUnqualComponentName (testName test)
        mkTestCall lbi clbi mainSrc otherSrcs =
          executableLike
            "test-suite"
            targetName
            lbi
            clbi
            bi
            mainSrc
            otherSrcs
            [("test_args", strList args) | let args = testSuiteArgs pkgDesc lbi test (Map.findWithDefault [] (packageName pkgDesc, componentName comp) projectTestOptions), not (null args)]

-- | The version of the build spec format; must match @SCHEMA_VERSION@ in
-- buck2\/cabal.bzl, which turns a spec into buck2 rules.
specSchemaVersion :: Int
specSchemaVersion = 1

-- | A package's build spec: everything the generated rules are built from
-- that comes from Cabal rather than from buck2 conventions (see
-- buck2\/cabal.bzl for the schema).
packageSpec :: PackageDescription -> FilePath -> [String] -> [Value] -> Value
packageSpec pkgDesc rootRelPkgDir projectGhcOptions components =
  VDict $
    [ ("schema", VInt specSchemaVersion)
    , ("package", VDict [("name", str (unPackageName (packageName pkgDesc))), ("dir", str rootRelPkgDir)])
    ]
      ++ listField "ghc_options" projectGhcOptions
      ++ [("components", VList components)]

-- | One component of a build spec. @extra@ holds what depends on the kind
-- of component (its sources, main module, test arguments).
specComponent :: LocalPackageIndex -> Set String -> String -> String -> BuildInfo -> [(String, Value)] -> Value
specComponent localIndex externalBuildTools kind name bi extra =
  VDict $
    [("kind", str kind), ("name", str name)]
      ++ extra
      ++ listField "ghc_options" (hcOptions GHC bi)
      ++ listField "cpp_options" (cppOptions bi)
      ++ [("language", str (prettyShow lang)) | Just lang <- [defaultLanguage bi]]
      ++ listField "extensions" (map prettyShow (defaultExtensions bi))
      ++ listField "extra_libraries" (extraLibs bi)
      ++ valuesField "deps" (map depSpec (libraryDeps localIndex bi))
      ++ valuesField "build_tools" (mapMaybe buildToolSpec (ordNub [(pn, exe) | ExeDependency pn exe _ <- buildToolDepends bi]))
      ++ listField "c_sources" (map getSymbolicPath (cSources bi))
      ++ listField "cxx_sources" (map getSymbolicPath (cxxSources bi))
      ++ listField "cxx_options" (cxxOptions bi)
      ++ listField "include_dirs" (map getSymbolicPath (includeDirs bi))
      ++ listField "pkgconfig" (ordNub [unPkgconfigName n | PkgconfigDependency n _ <- pkgconfigDepends bi])
  where
    depSpec (pn, ln) =
      VDict $
        [("package", str (unPackageName pn))]
          ++ [("library", str (unUnqualComponentName n)) | LSubLibName n <- [ln]]
          ++ [("dir", str dir) | Just (dir, _) <- [Map.lookup pn localIndex]]
    -- A tool that's neither built by this project nor resolved to a real
    -- external binary can't be put on PATH by buck2: dropped, silently -
    -- most build-tool-depends are Setup.hs-time tools nothing needs on PATH.
    buildToolSpec (pn, exe) = case Map.lookup pn localIndex of
      Just (dir, _) -> Just (VDict [("exe", str n), ("dir", str dir)])
      Nothing
        | n `Set.member` externalBuildTools -> Just (VDict [("exe", str n), ("external", VBool True)])
        | otherwise -> Nothing
      where
        n = unUnqualComponentName exe

-- | A list-valued field, omitted if empty.
listField :: String -> [String] -> [(String, Value)]
listField _ [] = []
listField k xs = [(k, strList xs)]

valuesField :: String -> [Value] -> [(String, Value)]
valuesField _ [] = []
valuesField k xs = [(k, VList xs)]

-- | The project-supplied GHC options (see 'ghcProgramArgs') for a
-- package. Cabal gives every component of a package the same ones (they
-- come from the package's @cabal.project@ stanza, not from anything
-- per-component), so any one component's will do; empty if none of its
-- components were configured at all.
packageGhcOptions :: PackageDescription -> Map (PackageName, ComponentName) LocalBuildInfo -> [String]
packageGhcOptions pkgDesc componentLBIs =
  fromMaybe [] . listToMaybe $
    [ ghcProgramArgs lbi
    | comp <- pkgBuildableComponents pkgDesc
    , Just (lbi, _) <- [lbiClbiFor pkgDesc componentLBIs comp]
    ]

-- | Extra GHC arguments the *project* supplies rather than the @.cabal@
-- file: @ghc-options:@ under @package@\/@program-options@ in
-- @cabal.project@, or @--ghc-options@ on the command line. Cabal applies
-- these to every GHC invocation for the component, after the @.cabal@
-- file's own @ghc-options@ (so they can override them) - taken from the
-- configured @ghc@ program in the 'LocalBuildInfo', which is exactly
-- where Cabal itself finds them, rather than reading the elaborated plan.
ghcProgramArgs :: LocalBuildInfo -> [String]
ghcProgramArgs lbi =
  -- cabal-install itself adds @-hide-all-packages@ to every GHC invocation
  -- (a workaround for custom Setup scripts that call GHC directly), which
  -- means nothing for a buck2 rule - the rule already passes it, and it's
  -- position-sensitive among GHC's package flags.
  filter (/= "-hide-all-packages") $
    maybe [] programOverrideArgs (lookupProgram ghcProgram (withPrograms lbi))

-- | The arguments @cabal test@ would pass to this test-suite's executable
-- for the project's @test-options:@ (\/@--test-options@), with the same
-- @$pkgid@\/@$test-suite@-style template variables expanded - Cabal's own
-- expansion (in "Distribution.Simple.Test.ExeV10") isn't exported.
testSuiteArgs :: PackageDescription -> LocalBuildInfo -> TestSuite -> [PathTemplate] -> [String]
testSuiteArgs pkgDesc lbi test = map (fromPathTemplate . substPathTemplate env)
  where
    env =
      initialPathTemplateEnv (packageId pkgDesc) (localUnitId lbi) (compilerInfo (compiler lbi)) (hostPlatform lbi)
        ++ [(TestSuiteNameVar, toPathTemplate (unUnqualComponentName (testName test)))]

-- | Writes this component's @cabal_macros.h@ to a real file next to the
-- package's own sources - exactly how real Cabal wires this up, just
-- generated ahead of time instead of by Setup.hs at configure time.
-- Written unconditionally, like real Cabal: harmless for a component
-- that never enables CPP (the header only matters if\/when cpp actually
-- runs). Returns the @cabal-buck2\/autogen\/BUCK@ export entry for it
-- (see 'PackageTargets'' own haddock); @cabal_component@ (in
-- buck2\/cabal.bzl's rules) is what wires the component up to include it.
writeMacrosHeader :: FilePath -> String -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> IO (String, FilePath)
writeMacrosHeader pkgDir targetName pkgDesc lbi clbi = do
  createDirectoryIfMissing True headerDir
  writeFile headerPath (generateCabalMacrosHeader pkgDesc lbi clbi)
  return (targetName ++ "-cabal-macros", exportRelPath)
  where
    exportRelPath = targetName </> "cabal_macros.h"
    headerDir = pkgDir </> "cabal-buck2" </> "autogen" </> targetName
    headerPath = headerDir </> "cabal_macros.h"

-- | The buck2 target name for one of a package's libraries: the package
-- name itself for the main (unnamed) library, matching every other
-- reference to it (@packages = [...]@, other packages' @build-depends@,
-- ...); the sub-library's own unqualified name otherwise - always unique
-- within one package's BUCK file, since Cabal itself already requires
-- every component name in a package to be distinct.
libTargetName :: PackageName -> LibraryName -> String
libTargetName pn LMainLibName = unPackageName pn
libTargetName _ (LSubLibName n) = unUnqualComponentName n

-- | Every library a component depends on: its @build-depends@ (each of which may name
-- more than one library of a package via @pkg:sublib@ - see
-- 'depLibraries'), closed over the reexports of local packages.
libraryDeps :: LocalPackageIndex -> BuildInfo -> [(PackageName, LibraryName)]
libraryDeps localIndex bi = closeOverReexports [] directDeps
  where
    directDeps =
      ordNub
        [ (depPkgName d, ln)
        | d <- targetBuildDepends bi
        , ln <- NES.toList (depLibraries d)
        ]
    -- A local package's buck2 haskell_library() rule only ever declares
    -- its own real source modules - unlike a real GHC package db entry,
    -- it has no way to also claim modules reexported (`reexported-
    -- modules:` in the .cabal file) from elsewhere. So a component that
    -- depends on a local package with reexports (e.g. `Cabal` re-
    -- exporting a chunk of `Cabal-syntax`) needs the reexport's origin
    -- package added as an explicit direct dependency too, or - once
    -- compile.bzl's `-hide-all-packages` is in effect - GHC can't find
    -- the reexported module at all: "Could not load module ...". This
    -- closure adds those origins (transitively, in case a reexporting
    -- package itself depends on another reexporting package).
    closeOverReexports seen [] = seen
    closeOverReexports seen (p@(pn, _) : rest)
      | p `elem` seen = closeOverReexports seen rest
      | otherwise =
          let origins = maybe [] snd (Map.lookup pn localIndex)
           in closeOverReexports (p : seen) (rest ++ [(o, LMainLibName) | o <- origins])

-- | A source file in a component's @srcs@: either a real file in the
-- package (relative to its directory), or one generated into the package's
-- @cabal-buck2\/autogen@ directory, named by its @export_file()@ entry there
-- (see 'PackageTargets').
data Src = SrcFile FilePath | SrcAutogen String

-- | As it appears in a build spec.
srcSpec :: Src -> Value
srcSpec (SrcFile p) = str p
srcSpec (SrcAutogen n) = VDict [("autogen", str n)]

-- | Resolve each module in @hs-source-dirs@ to its real file, trying
-- @.hs@\/@.lhs@\/@.hsc@ in turn (the extensions buck2/hsc2hs.bzl knows how
-- to handle) - returning @(moduleName, realRelativePath)@ pairs for the
-- dict form of @srcs@, which - unlike the plain-list form - is unaffected
-- by @hs-source-dirs@ not matching the BUCK package's own directory. The package's @Paths_<pkg>@ autogen module (if listed) is
-- special-cased: no such file exists anywhere - Cabal's own Setup.hs
-- generates it fresh on every real build - so 'writePathsModule' stands
-- one in ourselves rather than searching for it, which needs this
-- component's real 'LocalBuildInfo'\/'ComponentLocalBuildInfo' (see
-- 'lbiClbiFor') the same way 'writeMacrosHeader' does; 'Nothing' here (no
-- LBI available for this component) fails this module's own resolution,
-- same as a missing source file would. Also returns every
-- @cabal-buck2\/autogen\/BUCK@ export entry (see 'PackageTargets') picked
-- up along the way - in practice just the @Paths_\<pkg\>@ one, at most
-- once, if that module was among @mods@.
--
-- 'Nothing' if *any* module couldn't be resolved - the caller skips the
-- whole component in that case, rather than emitting a rule that
-- references a source file that doesn't exist: buck2 doesn't merely warn
-- about that, it fails outright while evaluating the @BUCK@ file, which
-- (since a @buck2 build //...@ evaluates every @BUCK@ file up front)
-- would otherwise take the *entire* build down over one unresolvable
-- module in one component of one package.
resolveModules :: Verbosity -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> FilePath -> BuildInfo -> [ModuleName.ModuleName] -> IO (Maybe ([(String, Src)], [(String, FilePath)]))
resolveModules verbosity pkgDesc mlbiClbi pkgDir bi mods = do
  results <- traverse (resolveOne verbosity pkgDesc mlbiClbi pkgDir (sourceDirs bi)) mods
  return $ case sequenceA results of
    Nothing -> Nothing
    Just triples -> Just ([(n, v) | (n, v, _) <- triples], concat [es | (_, _, es) <- triples])

resolveOne :: Verbosity -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> FilePath -> [FilePath] -> ModuleName.ModuleName -> IO (Maybe (String, Src, [(String, FilePath)]))
resolveOne verbosity pkgDesc mlbiClbi pkgDir dirs m
  | m == autogenPathsModuleName pkgDesc = case mlbiClbi of
      Nothing -> do
        warn verbosity $
          "cabal buck2: couldn't generate " ++ prettyShow m ++ " (no LocalBuildInfo available for this component)"
        return Nothing
      Just (lbi, clbi) -> do
        autogenExport <- writePathsModule pkgDir pkgDesc lbi clbi m
        return (Just (prettyShow m, SrcAutogen (fst autogenExport), [autogenExport]))
  | otherwise = do
      let modPath = ModuleName.toFilePath m
      -- buck2/haskell.bzl's own srcs-resolution (_resolve_src) auto-detects
      -- .hsc/.x/.y by the *source* file's extension and runs it through
      -- hsc2hs()/alex()/happy() - already loaded by haskell.bzl itself, so
      -- nothing extra needs to be loaded here for that to work.
      found <- firstExisting pkgDir dirs [modPath <.> ext | ext <- ["hs", "lhs", "hsc", "x", "y"]]
      case found of
        Just real -> return (Just (prettyShow m, SrcFile real, []))
        Nothing -> do
          warn verbosity $
            "cabal buck2: couldn't find a source file for module "
              ++ prettyShow m
              ++ " under "
              ++ intercalate ", " dirs
          return Nothing

-- | Cabal's own Setup.hs generates a @Paths_\<pkg\>@ module fresh at
-- configure\/build time (giving @version@\/@getDataFileName@\/etc) - no
-- real source file for it exists anywhere to find. Written here from
-- real Cabal's own 'generatePathsModule' (given this component's real
-- 'LocalBuildInfo'\/'ComponentLocalBuildInfo' - see 'lbiClbiFor'), so
-- install-dir\/relocatability logic matches a plain @cabal build@
-- exactly, instead of the hand-rolled @return "."@ stand-in this used to
-- be before a real 'LocalBuildInfo' was available here. Also returns its
-- own @cabal-buck2\/autogen\/BUCK@ export entry (see 'PackageTargets') -
-- written afresh, and so exported afresh, every time a component
-- happens to reference @Paths_\<pkg\>@, even though it's the same file
-- each time; 'PackageTargets''s own 'Semigroup' instance dedupes the
-- repeats away.
--
-- The @String@ returned for the caller's own @srcs@ entry is the
-- @export_file()@ target's label ('autogenExportLabel'), *not* a plain
-- file path: @cabal-buck2\/autogen\/@ has its own @BUCK@ file (written
-- by "Distribution.Client.Buck2.Generate"), so it's a different buck2
-- package from @pkgDir@ - a file living there can no longer be named by
-- a same-package-relative path from @pkgDir@'s own rules, only by a real
-- target reference (which @attrs.source()@, @srcs@'s own element type,
-- accepts just as well as a path).
writePathsModule :: FilePath -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName.ModuleName -> IO (String, FilePath)
writePathsModule pkgDir pkgDesc lbi clbi m = do
  createDirectoryIfMissing True (pkgDir </> "cabal-buck2" </> "autogen")
  writeFile (pkgDir </> relPath) (generatePathsModule pkgDesc lbi clbi)
  return (exportName, moduleFileName)
  where
    exportName = ModuleName.toFilePath m
    moduleFileName = exportName <.> "hs"
    relPath = "cabal-buck2" </> "autogen" </> moduleFileName

-- | A @detailed-0.9@ test-suite's own stub @Main@ - see 'testSuite's own
-- haddock for why this is a from-scratch driver over
-- @Distribution.TestSuite@'s public API, not real Cabal's own
-- @Setup.hs@-generated one (@Distribution.Simple.Test.LibV09.stubMain@,
-- which expects a handshake over stdin buck2 has no way to provide).
-- Returns its own @main_is@ source and @cabal-buck2\/autogen\/BUCK@
-- export entry the same way 'writePathsModule' does, and for the same
-- reason (a different buck2 package from @pkgDir@ once
-- @cabal-buck2\/autogen\/BUCK@ exists).
writeDetailedTestStub :: FilePath -> String -> ModuleName.ModuleName -> IO (Src, (String, FilePath))
writeDetailedTestStub pkgDir targetName testModule = do
  createDirectoryIfMissing True (pkgDir </> dir)
  writeFile (pkgDir </> relPath) contents
  return (SrcAutogen exportName, (exportName, exportRelPath))
  where
    exportName = targetName ++ "-stub-main"
    exportRelPath = targetName </> "Main.hs"
    dir = "cabal-buck2" </> "autogen" </> targetName
    relPath = dir </> "Main.hs"
    contents =
      unlines
        [ "-- @generated by `cabal buck2` - do not edit by hand."
        , "module Main (main) where"
        , ""
        , "import Distribution.TestSuite"
        , "import qualified " ++ prettyShow testModule ++ " as CabalBuck2TestModule"
        , "import System.Exit (ExitCode (..), exitWith)"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  ts <- CabalBuck2TestModule.tests"
        , "  oks <- mapM runTest ts"
        , "  exitWith (if and oks then ExitSuccess else ExitFailure 1)"
        , ""
        , "runTest :: Test -> IO Bool"
        , "runTest (Test ti) = run ti >>= report (name ti)"
        , "runTest (Group _ _ ts) = and <$> mapM runTest ts"
        , "runTest (ExtraOptions _ t) = runTest t"
        , ""
        , "report :: String -> Progress -> IO Bool"
        , "report n (Progress msg next) = putStrLn (n ++ \": \" ++ msg) >> next >>= report n"
        , "report n (Finished Pass) = putStrLn (n ++ \": PASS\") >> return True"
        , "report n (Finished (Fail msg)) = putStrLn (n ++ \": FAIL: \" ++ msg) >> return False"
        , "report n (Finished (Error msg)) = putStrLn (n ++ \": ERROR: \" ++ msg) >> return False"
        ]

-- | 'Nothing' if the main-is file couldn't be found - see 'resolveModules'
-- for why the caller must skip the whole component rather than emit a
-- rule pointing at a nonexistent file.
resolveMainIs :: Verbosity -> FilePath -> BuildInfo -> FilePath -> IO (Maybe String)
resolveMainIs verbosity pkgDir bi mainIs = do
  found <- firstExisting pkgDir (sourceDirs bi) [mainIs]
  case found of
    Just real -> return (Just real)
    Nothing -> do
      warn verbosity $
        "cabal buck2: couldn't find main-is file " ++ mainIs ++ " under " ++ intercalate ", " (sourceDirs bi)
      return Nothing

firstExisting :: FilePath -> [FilePath] -> [FilePath] -> IO (Maybe FilePath)
firstExisting pkgDir dirs candidates =
  listToMaybe . catMaybes
    <$> sequenceA
      [ do
        exists <- doesFileExist (pkgDir </> dir </> candidate)
        return (if exists then Just (dir </> candidate) else Nothing)
      | dir <- dirs
      , candidate <- candidates
      ]

sourceDirs :: BuildInfo -> [FilePath]
sourceDirs bi = case map getSymbolicPath (hsSourceDirs bi) of
  [] -> ["."]
  ds -> ds

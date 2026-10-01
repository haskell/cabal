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

-- | The generated rule calls for one package, plus the @load@ statements
-- they need at the top of the file, plus every generated autogen file
-- (a component's @cabal_macros.h@, or the package's own @Paths_\<pkg\>@
-- module) that needs an @export_file()@ entry in @cabal-buck2\/autogen\/
-- BUCK@ (see 'Distribution.Client.Buck2.Generate') so it can be
-- referenced - by a generated rule's own @cabal_component@ kwarg, or by a
-- hand-written rule elsewhere in the project - as a real, buck2-tracked
-- target instead of an untracked path string. Each entry is
-- @(exportTargetName, pathRelativeToCabalBuck2Autogen)@; 'writeMacrosHeader'
-- and 'writePathsModule' are the only producers.
data PackageTargets = PackageTargets
  { ptLoads :: [(String, [String])]
  , ptCalls :: [Call]
  , ptAutogenExports :: [(String, FilePath)]
  , ptConstants :: [(String, Value)]
  -- ^ Top-level bindings in the generated @.bzl@ that the calls refer to
  -- by name (see 'VVar'): currently just 'pkgGhcOptionsVar'.
  }

instance Semigroup PackageTargets where
  PackageTargets l1 c1 e1 k1 <> PackageTargets l2 c2 e2 k2 =
    PackageTargets (foldl' addLoad l1 l2) (c1 ++ c2) (ordNub (e1 ++ e2)) (k1 ++ k2)
    where
      addLoad acc (tgt, names) = case lookup tgt acc of
        Nothing -> acc ++ [(tgt, names)]
        Just _ -> map (\(t, ns) -> if t == tgt then (t, ordNub (ns ++ names)) else (t, ns)) acc

instance Monoid PackageTargets where
  mempty = PackageTargets [] [] [] []

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
  -- 'buildToolDependsArg'.
  -> Map (PackageName, ComponentName) [PathTemplate]
  -- ^ The project's @test-options:@ for each test-suite (see 'testSuiteArgs').
  -> FilePath
  -> PackageDescription
  -> IO PackageTargets
generatePackageTargets verbosity localIndex rootRelPkgDir componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc = do
  -- Computed up front (silently - see 'skippedLibraries's own haddock),
  -- so every component below - regardless of its own textual position
  -- in the .cabal file relative to the library it depends on - already
  -- knows which of this package's own libraries won't get a rule, and
  -- can skip itself too instead of emitting a dangling dependency edge.
  skippedLibs <- skippedLibraries verbosity pkgDesc rootRelPkgDir pkgDir componentLBIs
  targets <-
    mconcat
      <$> traverse
        (generateComponent verbosity localIndex rootRelPkgDir componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc skippedLibs)
        (pkgBuildableComponents pkgDesc)
  return
    targets
      { ptCalls = dedupPkgconfigCalls (ptCalls targets)
      , ptConstants = [(pkgGhcOptionsVar, strList opts) | let opts = packageGhcOptions pkgDesc componentLBIs, not (null opts)]
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
skippedLibraries :: Verbosity -> PackageDescription -> FilePath -> FilePath -> Map (PackageName, ComponentName) LocalBuildInfo -> IO (Set LibraryName)
skippedLibraries verbosity pkgDesc rootRelPkgDir pkgDir componentLBIs =
  Set.fromList . catMaybes
    <$> traverse checkLib [lib | CLib lib <- pkgBuildableComponents pkgDesc]
  where
    quiet = modifyVerbosityFlags (\vf -> vf{vLevel = Silent}) verbosity
    checkLib lib = do
      let bi = libBuildInfo lib
      msrcs <- resolveModules quiet pkgDesc (lbiClbiFor pkgDesc componentLBIs (CLib lib)) rootRelPkgDir pkgDir bi (exposedModules lib ++ otherModules bi)
      return $ if isNothing msrcs then Just (libName lib) else Nothing

-- | Look up a component's real, Cabal-computed 'LocalBuildInfo' (from
-- "Distribution.Client.CmdBuck2") plus its own 'ComponentLocalBuildInfo'
-- within it (via 'componentNameCLBIs') - 'Nothing' if either lookup fails
-- (e.g. a component that isn't part of the elaborated build plan, such as
-- a test-suite when tests aren't enabled), in which case callers skip the
-- component rather than emit a rule built from fabricated data.
lbiClbiFor :: PackageDescription -> Map (PackageName, ComponentName) LocalBuildInfo -> Component -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo)
lbiClbiFor pkgDesc componentLBIs comp = do
  lbi <- Map.lookup (packageName pkgDesc, componentName comp) componentLBIs
  clbi <- listToMaybe (componentNameCLBIs lbi (componentName comp))
  return (lbi, clbi)

-- | Two components of the *same* package sharing a @pkgconfig-depends@
-- each generate their own @external_pkgconfig_library()@ call (from
-- 'cxxLibraryFor', called once per component) - harmless on its own, but
-- both would declare the same target @name@ in the same
-- @generated_targets()@, which buck2 rejects as a duplicate target. Kept
-- as a post-pass here (rather than threading a running set through
-- component generation) so each component's own generation stays
-- self-contained; cross-*package* duplicates - two different local
-- packages needing the same system library - aren't addressed by this,
-- since each package's calls only ever collide with its own.
dedupPkgconfigCalls :: [Call] -> [Call]
dedupPkgconfigCalls = go []
  where
    go _ [] = []
    go seen (c : cs)
      | callFn c == "external_pkgconfig_library"
      , Just (VStr n) <- lookup "name" (callArgs c) =
          if n `elem` seen then go seen cs else c : go (n : seen) cs
      | otherwise = c : go seen cs

generateComponent
  :: Verbosity
  -> LocalPackageIndex
  -> FilePath
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -> Map (PackageName, ComponentName) [PathTemplate]
  -> FilePath
  -> PackageDescription
  -> Set LibraryName
  -> Component
  -> IO PackageTargets
generateComponent verbosity localIndex rootRelPkgDir componentLBIs externalBuildTools projectTestOptions pkgDir pkgDesc skippedLibs comp = case comp of
  CLib lib -> library (libTargetName (packageName pkgDesc) (libName lib)) lib
  CExe exe -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (exeName exe)) "executable" $ executable exe
  CTest test -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (testName test)) "test-suite" $ testSuite test
  CBench bench -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (benchmarkName bench)) "benchmark" $ benchmark bench
  CFLib _ -> skip "foreign library (not supported yet)"
  where
    skip why = do
      warn verbosity $ "cabal buck2: skipping " ++ why ++ " in package " ++ unPackageName (packageName pkgDesc)
      return mempty

    usesPkgGhcOptions = not (null (packageGhcOptions pkgDesc componentLBIs))

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
        msrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (exposedModules lib ++ otherModules bi)
        case msrcs of
          Nothing -> skip ("library " ++ targetName ++ " (couldn't resolve all its modules)")
          Just (srcs, srcAutogenExports) -> do
            (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex rootRelPkgDir pkgDir targetName bi
            macrosExport <- writeMacrosHeader pkgDir targetName pkgDesc lbi clbi
            let (pkgs, deps) = classifyDeps localIndex bi
                hlCall =
                  call
                    "haskell_library"
                    ( [ ("name", str targetName)
                      , ("srcs", VDict srcs)
                      , cabalComponentArg rootRelPkgDir targetName
                      ]
                        ++ compilerFlagsArg usesPkgGhcOptions bi
                        ++ exportedLinkerFlagsArg bi
                        ++ optionalListArg "packages" pkgs
                        ++ optionalListArg "deps" (deps ++ cxxDeps)
                        ++ buildToolDependsArg localIndex externalBuildTools bi
                        ++ [("visibility", strList ["PUBLIC"])]
                    )
            return $
              PackageTargets
                (("//buck2:haskell.bzl", ["haskell_library"]) : cxxLoads)
                (cxxCalls ++ [hlCall])
                (macrosExport : srcAutogenExports)
                []

    executable exe = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip ("executable " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> do
        mmainSrc <- resolveMainIs verbosity pkgDir bi (getSymbolicPath (modulePath exe))
        motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
        case (mmainSrc, motherSrcs) of
          (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) -> do
            (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex rootRelPkgDir pkgDir targetName bi
            macrosExport <- writeMacrosHeader pkgDir targetName pkgDesc lbi clbi
            let (pkgs, deps) = classifyDeps localIndex bi
                binCall =
                  call
                    "haskell_binary"
                    ( [ ("name", str targetName)
                      , ("srcs", VDict (("Main", str mainSrc0) : otherSrcs))
                      , cabalComponentArg rootRelPkgDir targetName
                      ]
                        ++ compilerFlagsArg usesPkgGhcOptions bi
                        ++ linkerFlagsArg usesPkgGhcOptions bi
                        ++ optionalListArg "packages" pkgs
                        ++ optionalListArg "deps" (deps ++ cxxDeps)
                        ++ buildToolDependsArg localIndex externalBuildTools bi
                        ++ [("visibility", strList ["PUBLIC"])]
                    )
            return $
              PackageTargets
                (("//buck2:haskell.bzl", ["haskell_binary"]) : cxxLoads)
                (cxxCalls ++ [binCall])
                (macrosExport : srcAutogenExports)
                []
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
          motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
          case (mmainSrc, motherSrcs) of
            (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) -> do
              (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex rootRelPkgDir pkgDir targetName bi
              macrosExport <- writeMacrosHeader pkgDir targetName pkgDesc lbi clbi
              let (pkgs, deps) = classifyDeps localIndex bi
                  binCall =
                    call
                      "haskell_binary"
                      ( [ ("name", str targetName)
                        , ("srcs", VDict (("Main", str mainSrc0) : otherSrcs))
                        , cabalComponentArg rootRelPkgDir targetName
                        ]
                          ++ compilerFlagsArg usesPkgGhcOptions bi
                          ++ linkerFlagsArg usesPkgGhcOptions bi
                          ++ optionalListArg "packages" pkgs
                          ++ optionalListArg "deps" (deps ++ cxxDeps)
                          ++ buildToolDependsArg localIndex externalBuildTools bi
                          ++ [("visibility", strList ["PUBLIC"])]
                      )
              return $
                PackageTargets
                  (("//buck2:haskell.bzl", ["haskell_binary"]) : cxxLoads)
                  (cxxCalls ++ [binCall])
                  (macrosExport : srcAutogenExports)
                  []
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
          motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
          case (mmainSrc, motherSrcs) of
            (Just mainSrc0, Just (otherSrcs, srcAutogenExports)) ->
              mkTestCall lbi clbi (str mainSrc0) otherSrcs srcAutogenExports
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
          mtestModSrc <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi [testModule]
          motherSrcs <- resolveModules verbosity pkgDesc (Just (lbi, clbi)) rootRelPkgDir pkgDir bi (otherModules bi)
          case (mtestModSrc, motherSrcs) of
            (Just (testModSrc, testModAutogenExports), Just (otherSrcs, otherAutogenExports)) -> do
              (stubLabel, stubAutogenExport) <- writeDetailedTestStub rootRelPkgDir pkgDir targetName testModule
              mkTestCall
                lbi
                clbi
                (str stubLabel)
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
        mkTestCall lbi clbi mainSrc otherSrcs srcAutogenExports = do
          (cxxLoads, cxxDeps, cxxCalls) <- cxxLibraryFor localIndex rootRelPkgDir pkgDir targetName bi
          macrosExport <- writeMacrosHeader pkgDir targetName pkgDesc lbi clbi
          let (pkgs, deps) = classifyDeps localIndex bi
              testCall =
                call
                  "haskell_test"
                  ( [ ("name", str targetName)
                    , ("srcs", VDict (("Main", mainSrc) : otherSrcs))
                    , cabalComponentArg rootRelPkgDir targetName
                    , -- Real `cabal test` always runs a test-suite with its
                      -- cwd set to the package's own directory - matched
                      -- here so a test that reads its own fixture files by
                      -- a package-relative path (extremely common) works
                      -- the same way under buck2 (see haskell_test()'s own
                      -- haddock in buck2/haskell.bzl for why this needs a
                      -- generated wrapper, not just a plain attr).
                      ("cwd", str rootRelPkgDir)
                    ]
                      -- Not 'optionalListArg': it de-duplicates, which would
                      -- corrupt a repeated option or its value.
                      ++ [("test_args", strList args) | let args = testSuiteArgs pkgDesc lbi test (Map.findWithDefault [] (packageName pkgDesc, componentName comp) projectTestOptions), not (null args)]
                      ++ compilerFlagsArg usesPkgGhcOptions bi
                      ++ linkerFlagsArg usesPkgGhcOptions bi
                      ++ optionalListArg "packages" pkgs
                      ++ optionalListArg "deps" (deps ++ cxxDeps)
                      ++ buildToolDependsArg localIndex externalBuildTools bi
                  )
          return $
            PackageTargets
              (("//buck2:haskell.bzl", ["haskell_test"]) : cxxLoads)
              (cxxCalls ++ [testCall])
              (macrosExport : srcAutogenExports)
              []

-- | @ghc-options@ + @cpp-options@ + @default-extensions@ (as @-X...@
-- flags), the sources of per-component GHC flags buck2's @compiler_flags@
-- covers. @other-extensions@ is deliberately excluded: those are declared
-- via in-module @LANGUAGE@ pragmas, not enabled component-wide. The
-- @cabal_macros.h@ @-optP-include@ flags are *not* here any more - they're
-- injected by buck2\/haskell.bzl's own @cabal_component@ kwarg (see
-- 'cabalComponentArg'), which - unlike a plain string folded into this
-- list - can be a real, buck2-tracked dependency edge.
compilerFlagsArg :: Bool -> BuildInfo -> [(String, Value)]
compilerFlagsArg usesPkgGhcOptions bi =
  flagsArg "compiler_flags" usesPkgGhcOptions (hcOptions GHC bi ++ cppOptions bi ++ languageFlag ++ extensionFlags)
  where
    -- default-language isn't just documentation: GHC2021/GHC2024 each
    -- imply a large bundle of extensions (TypeApplications among them) -
    -- omitting this made GHC silently fall back to its own default
    -- (Haskell2010) instead, which is how Cabal-syntax's actual use of
    -- TypeApplications (implied by its own `default-language: GHC2021`,
    -- with nothing naming TypeApplications directly) went unnoticed
    -- until a real `buck2 build` on it failed outright.
    languageFlag = ["-X" ++ prettyShow lang | lang <- maybeToList (defaultLanguage bi)]
    extensionFlags = ["-X" ++ prettyShow ext | ext <- defaultExtensions bi]

-- | @extra-libraries@ on a library, as @-l@ flags on its
-- @exported_linker_flags@
exportedLinkerFlagsArg :: BuildInfo -> [(String, Value)]
exportedLinkerFlagsArg bi = optionalListArg "exported_linker_flags" ["-l" ++ lib | lib <- extraLibs bi]

-- | @extra-libraries@ (as @-l@ flags) plus @ghc-options@ on an
-- executable\/test-suite's plain @linker_flags@ - correct as-is here
-- (unlike on a library): both rules already apply @linker_flags@ directly
-- to their own, one and only, final executable link. @ghc-options@ is
-- included here too, not just in @compiler_flags@: buck2's haskell_binary
-- only passes @compiler_flags@ to each module's own compile step, not the
-- final link (a real @ghc -o@ invocation, distinct from that), whereas
-- Cabal applies a component's whole @ghc-options@ to every ghc invocation
-- for it, compile and link alike - so anything there that's actually
-- link-relevant (e.g. @-threaded@\/@-rtsopts@, which select the RTS
-- linked in) needs to reach buck2's link step explicitly via
-- @linker_flags@, or it's silently dropped. A compile-only flag showing
-- up here too (e.g. @-Wall@) is harmless: GHC's link-mode invocation
-- just ignores flags that don't apply to it.
linkerFlagsArg :: Bool -> BuildInfo -> [(String, Value)]
linkerFlagsArg usesPkgGhcOptions bi = flagsArg "linker_flags" usesPkgGhcOptions (["-l" ++ lib | lib <- extraLibs bi] ++ hcOptions GHC bi)

-- | The name of the file-level constant holding 'packageGhcOptions'.
pkgGhcOptionsVar :: String
pkgGhcOptionsVar = "GHC_OPTIONS"

-- | A flags kwarg: the component's own flags, followed - if the package
-- has any project-supplied GHC options - by a reference to the
-- file-level constant holding them, so they're written out once per
-- package rather than once per rule. Cabal puts these after the
-- component's own @ghc-options@, so they can override them.
flagsArg :: String -> Bool -> [String] -> [(String, Value)]
flagsArg name usesPkgGhcOptions own = case (ordNub own, usesPkgGhcOptions) of
  ([], False) -> []
  ([], True) -> [(name, VVar pkgGhcOptionsVar)]
  (xs, False) -> [(name, strList xs)]
  (xs, True) -> [(name, VConcat [strList xs, VVar pkgGhcOptionsVar])]

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
-- (see 'PackageTargets'' own haddock) - 'cabalComponentArg' is what
-- actually wires the generated component up to include it.
writeMacrosHeader :: FilePath -> String -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> IO (String, FilePath)
writeMacrosHeader pkgDir targetName pkgDesc lbi clbi = do
  createDirectoryIfMissing True headerDir
  writeFile headerPath (generateCabalMacrosHeader pkgDesc lbi clbi)
  return (targetName ++ "-cabal-macros", exportRelPath)
  where
    exportRelPath = targetName </> "cabal_macros.h"
    headerDir = pkgDir </> "cabal-buck2" </> "autogen" </> targetName
    headerPath = headerDir </> "cabal_macros.h"

-- | The @cabal_component = (pkg, component)@ kwarg understood by
-- buck2\/haskell.bzl's @haskell_library()@\/@haskell_binary()@ (and, via
-- that, @haskell_test()@ - see its own comment there): tells the rule
-- which @cabal-buck2\/autogen\/BUCK@ export target holds *this*
-- component's own @cabal_macros.h@, so it can inject
-- @-optP-include -optP$(location ...)@ itself, as a real dependency edge
-- (unlike a plain path string folded into @compiler_flags@, which isn't
-- buck2-tracked at all). @pkg@ must
-- match 'localTargetLabel''s own directory convention (@.@ at the
-- project root) - haskell.bzl computes the matching @export_file()@
-- label (@\/\/pkg\/cabal-buck2\/autogen:component-cabal-macros@) the
-- same way "Distribution.Client.Buck2.Generate" lays out
-- @cabal-buck2\/autogen\/BUCK@ itself.
cabalComponentArg :: FilePath -> String -> (String, Value)
cabalComponentArg rootRelPkgDir targetName =
  ("cabal_component", VTuple [str rootRelPkgDir, str targetName])

optionalListArg :: String -> [String] -> [(String, Value)]
optionalListArg _ [] = []
optionalListArg name xs = [(name, strList (ordNub xs))]

-- | The buck2 target name for one of a package's libraries: the package
-- name itself for the main (unnamed) library, matching every other
-- reference to it (@packages = [...]@, other packages' @build-depends@,
-- ...); the sub-library's own unqualified name otherwise - always unique
-- within one package's BUCK file, since Cabal itself already requires
-- every component name in a package to be distinct.
libTargetName :: PackageName -> LibraryName -> String
libTargetName pn LMainLibName = unPackageName pn
libTargetName _ (LSubLibName n) = unUnqualComponentName n

-- | Split a component's @build-depends@ (each of which may name one or
-- more specific sub-libraries of a package via @pkg:sublib@ - see
-- 'depLibraries') into external *main*-library package names (fed to
-- buck2/haskell.bzl's @packages =@ convenience param - which only ever
-- resolves a package's main library, per its own haddock in
-- buck2/haskell.bzl) and target labels (fed to @deps =@): a local
-- package's own sub-library target (@//dir:sublib@, via 'libTargetName')
-- when one was named, an *external* package's own named sub-library
-- target (@//third-party/haskell:sublib@ - "Distribution.Client.
-- Buck2.Prebuilt" generates one @haskell_prebuilt_library()@ per library
-- unit there too, target-named the exact same way via the same
-- 'libTargetName', not one per package name) when one was named there
-- instead, or nothing at all for an ordinary external main-library
-- dependency (that one's covered by @packages =@ already).
classifyDeps :: LocalPackageIndex -> BuildInfo -> ([String], [String])
classifyDeps localIndex bi =
  ( ordNub [unPackageName pn | (pn, LMainLibName) <- allDeps, not (Map.member pn localIndex)]
  , ordNub $
      [localTargetLabel dir (libTargetName pn ln) | (pn, ln) <- allDeps, Just (dir, _) <- [Map.lookup pn localIndex]]
        ++ [ thirdPartyHaskellTargetLabel (libTargetName pn ln)
           | (pn, ln@(LSubLibName _)) <- allDeps
           , not (Map.member pn localIndex)
           ]
  )
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
    allDeps = closeOverReexports [] directDeps
    closeOverReexports seen [] = seen
    closeOverReexports seen (p@(pn, _) : rest)
      | p `elem` seen = closeOverReexports seen rest
      | otherwise =
          let origins = maybe [] snd (Map.lookup pn localIndex)
           in closeOverReexports (p : seen) (rest ++ [(o, LMainLibName) | o <- origins])

thirdPartyHaskellTargetLabel :: String -> String
thirdPartyHaskellTargetLabel name = "third-party-haskell//:" ++ name

localTargetLabel :: FilePath -> String -> String
localTargetLabel dir targetName = "//" ++ (if dir == "." then "" else dir) ++ ":" ++ targetName

-- | @build_tool_depends = [...]@ - one real buck2 target label per
-- @build-tool-depends:@ executable this component declares, letting
-- buck2\/prelude\/haskell\/compile.bzl's own @compile()@ put each one on
-- @PATH@ for exactly this rule's own compile actions (see that attr's
-- own haddock in @buck2\/prelude\/decls\/haskell_common.bzl@) - not a
-- single project-wide directory shared by every component.
--
-- A build-tool-depends naming a *local* package's own executable
-- component references that component's real, already-generated
-- @haskell_binary()@ target directly (the exact same label 'executable'
-- itself produces for it: @unUnqualComponentName exe@ is, by
-- construction, that function's own @targetName@ too) - buck2 then
-- builds it as a genuine dependency, unlike the old host-filesystem-
-- symlink approach, which only ever worked for already-installed
-- *external* dependencies (a local one doesn't exist on disk at all
-- until buck2 itself builds it). An external dependency instead
-- references the @export_file()@-wrapped target
-- "Distribution.Client.Buck2.Prebuilt" generates for it (see
-- 'generatePrebuilt' - @externalBuildTools@ here is exactly the set of
-- names it resolved a real binary for and generated a target for).
--
-- An entry naming a tool that's neither a local package's own
-- executable nor a resolved external one is silently omitted - most
-- @build-tool-depends@ executables are ordinary Setup.hs-time tools
-- with nothing to do with GHC needing them on @PATH@, so a miss here is
-- the overwhelmingly common, unremarkable case (the same reasoning
-- "Distribution.Client.Buck2.Prebuilt"'s own resolution step already
-- used); a real @buck2 build@ still fails exactly the way it always did
-- if the omission actually mattered (a bare tool name GHC can't find).
--
-- Known gap, shared with 'classifyDeps's own @deps@ list: if the local
-- executable a build-tool-depends names is itself skipped (see
-- 'skippedLibraries's own reasoning, which only covers *libraries*),
-- this would reference a target that was never generated - unlike the
-- library case, not currently guarded against. Not a regression versus
-- the old design (which could never reference a local build-tool-depends
-- executable at all, skipped or not - the bug this rewrite exists to
-- fix), so left as a known, pre-existing class of limitation rather than
-- solved here.
buildToolDependsArg :: LocalPackageIndex -> Set String -> BuildInfo -> [(String, Value)]
buildToolDependsArg localIndex externalBuildTools bi =
  optionalListArg "build_tool_depends" (mapMaybe resolve (ordNub [(pn, exe) | ExeDependency pn exe _ <- buildToolDepends bi]))
  where
    resolve (pn, exe) = case Map.lookup pn localIndex of
      Just (dir, _) -> Just (localTargetLabel dir (unUnqualComponentName exe))
      Nothing
        | unUnqualComponentName exe `Set.member` externalBuildTools ->
            Just (thirdPartyHaskellTargetLabel (unUnqualComponentName exe ++ "-exe"))
        | otherwise -> Nothing

-- | The target label for one @cabal-buck2\/autogen\/BUCK@ export (see
-- 'PackageTargets') - needed (not just the plain file path) to reference
-- an autogen file from @srcs@ once it has its own @export_file()@ entry:
-- @cabal-buck2\/autogen\/@ is a *different* buck2 package from @pkgDir@
-- itself (having its own @BUCK@ file is what makes a directory a
-- package), so a file living there is no longer a plain same-package
-- source path as far as @pkgDir@'s own rules are concerned, even though
-- it's still on disk right where it always was.
autogenExportLabel :: FilePath -> String -> String
autogenExportLabel rootRelPkgDir = localTargetLabel autogenDir
  where
    autogenDir
      | rootRelPkgDir == "." = "cabal-buck2" </> "autogen"
      | otherwise = rootRelPkgDir </> "cabal-buck2" </> "autogen"

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
resolveModules :: Verbosity -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> FilePath -> FilePath -> BuildInfo -> [ModuleName.ModuleName] -> IO (Maybe ([(String, Value)], [(String, FilePath)]))
resolveModules verbosity pkgDesc mlbiClbi rootRelPkgDir pkgDir bi mods = do
  results <- traverse (resolveOne verbosity pkgDesc mlbiClbi rootRelPkgDir pkgDir (sourceDirs bi)) mods
  return $ case sequenceA results of
    Nothing -> Nothing
    Just triples -> Just ([(n, v) | (n, v, _) <- triples], concat [es | (_, _, es) <- triples])

resolveOne :: Verbosity -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> FilePath -> FilePath -> [FilePath] -> ModuleName.ModuleName -> IO (Maybe (String, Value, [(String, FilePath)]))
resolveOne verbosity pkgDesc mlbiClbi rootRelPkgDir pkgDir dirs m
  | m == autogenPathsModuleName pkgDesc = case mlbiClbi of
      Nothing -> do
        warn verbosity $
          "cabal buck2: couldn't generate " ++ prettyShow m ++ " (no LocalBuildInfo available for this component)"
        return Nothing
      Just (lbi, clbi) -> do
        (label, autogenExport) <- writePathsModule rootRelPkgDir pkgDir pkgDesc lbi clbi m
        return (Just (prettyShow m, str label, [autogenExport]))
  | otherwise = do
      let modPath = ModuleName.toFilePath m
      -- buck2/haskell.bzl's own srcs-resolution (_resolve_src) auto-detects
      -- .hsc/.x/.y by the *source* file's extension and runs it through
      -- hsc2hs()/alex()/happy() - already loaded by haskell.bzl itself, so
      -- nothing extra needs to be loaded here for that to work.
      found <- firstExisting pkgDir dirs [modPath <.> ext | ext <- ["hs", "lhs", "hsc", "x", "y"]]
      case found of
        Just real -> return (Just (prettyShow m, str real, []))
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
writePathsModule :: FilePath -> FilePath -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName.ModuleName -> IO (String, (String, FilePath))
writePathsModule rootRelPkgDir pkgDir pkgDesc lbi clbi m = do
  createDirectoryIfMissing True (pkgDir </> "cabal-buck2" </> "autogen")
  writeFile (pkgDir </> relPath) (generatePathsModule pkgDesc lbi clbi)
  return (autogenExportLabel rootRelPkgDir exportName, (exportName, moduleFileName))
  where
    exportName = ModuleName.toFilePath m
    moduleFileName = exportName <.> "hs"
    relPath = "cabal-buck2" </> "autogen" </> moduleFileName

-- | A @detailed-0.9@ test-suite's own stub @Main@ - see 'testSuite's own
-- haddock for why this is a from-scratch driver over
-- @Distribution.TestSuite@'s public API, not real Cabal's own
-- @Setup.hs@-generated one (@Distribution.Simple.Test.LibV09.stubMain@,
-- which expects a handshake over stdin buck2 has no way to provide).
-- Returns its own @srcs@-entry label and @cabal-buck2\/autogen\/BUCK@
-- export entry the same way 'writePathsModule' does, and for the same
-- reason (a different buck2 package from @pkgDir@ once
-- @cabal-buck2\/autogen\/BUCK@ exists).
writeDetailedTestStub :: FilePath -> FilePath -> String -> ModuleName.ModuleName -> IO (String, (String, FilePath))
writeDetailedTestStub rootRelPkgDir pkgDir targetName testModule = do
  createDirectoryIfMissing True (pkgDir </> dir)
  writeFile (pkgDir </> relPath) contents
  return (autogenExportLabel rootRelPkgDir exportName, (exportName, exportRelPath))
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

-- | A 'cxx_library' for a component's @cxx-sources@\/@c-sources@, plus an
-- @external_pkgconfig_library@ for each distinct @pkgconfig-depends@ it
-- needs - or nothing at all if the component has no C\/C++ sources.
cxxLibraryFor
  :: LocalPackageIndex
  -> FilePath
  -> FilePath
  -> String
  -> BuildInfo
  -> IO ([(String, [String])], [String], [Call])
cxxLibraryFor _localIndex rootRelPkgDir _pkgDir targetName bi
  | null srcs = return ([], [], [])
  | otherwise =
      return
        ( ("//buck2:cxx.bzl", ["cxx_library"])
            : [("@prelude//third-party:pkgconfig.bzl", ["external_pkgconfig_library"]) | not (null pkgconfigNames)]
        , [":" ++ cxxTargetName]
        , pkgconfigCalls ++ [cxxCall]
        )
  where
    srcs = map getSymbolicPath (cSources bi ++ cxxSources bi)
    cxxTargetName = targetName ++ "-cxx"
    -- Unlike @srcs@ (an @attrs.source()@, resolved by buck2 itself
    -- relative to this rule's own package - see 'cxxCall's own @srcs@),
    -- @exported_preprocessor_flags@ is a plain @attrs.arg()@ string list
    -- - buck2 has no idea @-I<path>@ names a path at all, let alone one
    -- that needs resolving relative to anything, so a bare
    -- package-relative @include-dirs:@ entry (e.g. @cbits@) needs
    -- @rootRelPkgDir@ folded in by hand here, the same way
    -- 'writeMacrosHeader'\/'cabalComponentArg' already do for other
    -- flag-embedded paths - every cxx action in this project always runs
    -- with the *project root* as its cwd (confirmed empirically: a real
    -- @buck2 build@ of a non-root package's c-sources with a bare
    -- @-Icbits@ here fails outright, "file not found", since that
    -- resolves to @\<root\>\/cbits@ instead of
    -- @\<root\>\/\<pkgDir\>\/cbits@).
    includeFlags = ["-I" ++ (if rootRelPkgDir == "." then d else rootRelPkgDir </> d) | dir <- includeDirs bi, let d = getSymbolicPath dir]
    pkgconfigNames = ordNub [unPkgconfigName n | PkgconfigDependency n _ <- pkgconfigDepends bi]
    pkgconfigCalls =
      [ call
        "external_pkgconfig_library"
        [("name", str ("pkgconfig-" ++ n)), ("package", str n), ("visibility", strList ["PUBLIC"])]
      | n <- pkgconfigNames
      ]
    -- buck2/cxx.bzl's cxx_library() wrapper adds -std=c++20 to
    -- compiler_flags whenever cxx_std isn't explicitly turned off - and
    -- that flag applies to every source in the target, C included, so a
    -- component with c-sources but no cxx-sources needs it turned off
    -- entirely (clang/gcc reject -std=c++20 for a plain .c compile).
    cxxCall =
      call
        "cxx_library"
        ( [ ("name", str cxxTargetName)
          , ("srcs", strList srcs)
          ]
            ++ optionalListArg "exported_preprocessor_flags" includeFlags
            ++ optionalListArg "compiler_flags" (cxxOptions bi)
            ++ optionalListArg "deps" [":pkgconfig-" ++ n | n <- pkgconfigNames]
            ++ [("visibility", strList ["PUBLIC"])]
            ++ [("cxx_std", VBool False) | null (cxxSources bi)]
        )

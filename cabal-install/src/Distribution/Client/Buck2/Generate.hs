-- | Generate all the info that buck2 needs to build the components of
-- a package, as a pure function from the 'PackageDescription',
-- 'LocalBuildInfo'(s) and a few other things.
--
-- The buck2 build spec is described by the data types in
-- "Distribution.Client.Buck2.Spec", and will be emitted into
-- @BUCK.cabal.bzl@ and interpreted by @buck2@ to produce the final
-- build targets (the interpreter is @buck2/cabal.bzl@). It's done
-- this way rather than emitting targets directly so that a custom
-- @BUCK@ file can override or customise the targets.
--
-- Here we also produce the content for the autogen files, such as
-- @cabal_macros.h@ and @Paths_<pkg>.hs@.
--
-- All the content we generate here will be written to files later in
-- "Distribution.Client.Buck2.Write".
module Distribution.Client.Buck2.Generate
  ( LocalPackageIndex
  , AutogenFile (..)
  , PackageTargets (..)
  , generatePackageTargets
  , sourceCandidates
  , libTargetName
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import System.FilePath ((<.>), (</>))

import Data.Either (fromLeft)
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
  , cSources
  , cppOptions
  , cxxOptions
  , cxxSources
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
import Distribution.Simple.Utils (ordNub)

import Distribution.Client.Buck2.Spec

-- | Maps every local project package's name to the buck2 cell-relative
-- directory its @BUCK@ file lives in (@.@ for one at the project root), so
-- a dependency on another local package can be turned into a fully
-- qualified target label - plus the set of other packages its main
-- library's @reexported-modules@ re-export from (see 'classifyDeps').
type LocalPackageIndex = Map PackageName (FilePath, [PackageName])

-- | A generated file that lives in the package's @cabal-buck2\/autogen@
-- directory (a component's @cabal_macros.h@, the package's own
-- @Paths_\<pkg\>@ module, a @detailed-0.9@ test-suite's stub @Main@), and
-- needs an @export_file()@ entry in @cabal-buck2\/autogen\/BUCK@ (see
-- "Distribution.Client.Buck2.Write", which writes both) so it can be
-- referenced - by a component's @cabal_component@, or by a hand-written rule
-- elsewhere in the project - as a real, buck2-tracked target instead of an
-- untracked path string.
data AutogenFile = AutogenFile
  { autogenName :: String
  -- ^ The name of its @export_file()@ target.
  , autogenPath :: FilePath
  -- ^ Relative to @cabal-buck2\/autogen@.
  , autogenContents :: String
  }
  deriving (Eq, Ord)

-- | What is generated for one package: its build spec,
-- and the autogen files it needs.
data PackageTargets = PackageTargets
  { ptSpec :: BuildSpec
  , ptComponentCount :: Int
  , ptAutogenFiles :: [AutogenFile]
  }

-- | One component's contribution to a 'PackageTargets': its entry in the
-- spec (none if it was skipped), its autogen files, and the warnings
-- produced along the way.
data ComponentTargets = ComponentTargets
  { ctSpec :: [SpecComponent]
  , ctAutogen :: [AutogenFile]
  , ctWarnings :: [String]
  }

instance Semigroup ComponentTargets where
  ComponentTargets s1 a1 w1 <> ComponentTargets s2 a2 w2 = ComponentTargets (s1 ++ s2) (a1 ++ a2) (w1 ++ w2)

instance Monoid ComponentTargets where
  mempty = ComponentTargets [] [] []

componentTargets :: SpecComponent -> [AutogenFile] -> ComponentTargets
componentTargets component files = mempty{ctSpec = [component], ctAutogen = files}

-- | Generate the buck2 targets for every buildable component of a local
-- package, along with the warnings for the components that had to be
-- skipped (and why). This does no IO: the source files it looks for are
-- given as a set of paths (relative to the package's directory), which
-- must include every path 'sourceCandidates' lists that exists.
generatePackageTargets
  :: LocalPackageIndex
  -> FilePath
  -- ^ The package's own buck2 cell-relative directory (@.@ at the project
  -- root).
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -- ^ Every @build-tool-depends:@ executable name
  -- "Distribution.Client.Buck2.Prebuilt" resolved a real *external*
  -- binary for (and generated an @export_file()@ target for) - see
  -- 'specComponent'.
  -> Map (PackageName, ComponentName) [PathTemplate]
  -- ^ The project's @test-options:@ for each test-suite (see 'testSuiteArgs').
  -> Set FilePath
  -> PackageDescription
  -> (Maybe PackageTargets, [String])
generatePackageTargets localIndex rootRelPkgDir componentLBIs externalBuildTools projectTestOptions sources pkgDesc =
  (targets, concatMap ctWarnings results)
  where
    generate = generateComponent localIndex componentLBIs externalBuildTools projectTestOptions sources pkgDesc
    comps = pkgBuildableComponents pkgDesc

    -- Worked out ahead of the other components, so that one that
    -- build-depends on a library of this package that won't get a rule
    -- (unresolvable modules) can skip itself too, instead of emitting a
    -- rule whose @deps@ references a target that was never generated -
    -- buck2 fails that outright at analysis time ("Unknown target"), for
    -- the *entire* build, the same class of problem 'resolveModules'
    -- exists to avoid for a single component's own missing source file.
    -- Libraries themselves never depend on this set.
    skippedLibs =
      Set.fromList [libName lib | comp@(CLib lib) <- comps, null (ctSpec (generate Set.empty comp))]

    results = map (generate skippedLibs) comps
    components = concatMap ctSpec results
    targets
      | null components = Nothing
      | otherwise =
          Just
            PackageTargets
              { ptSpec = packageSpec pkgDesc rootRelPkgDir (packageGhcOptions pkgDesc componentLBIs) components
              , ptComponentCount = length components
              , ptAutogenFiles = dedupAutogenFiles (concatMap ctAutogen results)
              }

-- | Look up a component's 'LocalBuildInfo' and
-- 'ComponentLocalBuildInfo' -- 'Nothing' if either lookup fails
-- (e.g. a component that isn't part of the elaborated build plan,
-- such as a test-suite when tests aren't enabled), in which case
-- callers skip the component.
lbiClbiFor :: PackageDescription -> Map (PackageName, ComponentName) LocalBuildInfo -> Component -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo)
lbiClbiFor pkgDesc componentLBIs comp = do
  lbi <- Map.lookup (packageName pkgDesc, componentName comp) componentLBIs
  clbi <- listToMaybe (componentNameCLBIs lbi (componentName comp))
  return (lbi, clbi)

-- | Two components referencing the same autogen file (the package's
-- @Paths_\<pkg\>@ module) each produce it; keep one entry per target, at the
-- position of the first, with the contents of the last.
dedupAutogenFiles :: [AutogenFile] -> [AutogenFile]
dedupAutogenFiles files =
  [ f{autogenContents = latest Map.! autogenName f}
  | f <- nubBy ((==) `on` autogenName) files
  ]
  where
    latest = Map.fromList [(autogenName f, autogenContents f) | f <- files]

generateComponent
  :: LocalPackageIndex
  -> Map (PackageName, ComponentName) LocalBuildInfo
  -> Set String
  -> Map (PackageName, ComponentName) [PathTemplate]
  -> Set FilePath
  -> PackageDescription
  -> Set LibraryName
  -> Component
  -> ComponentTargets
generateComponent localIndex componentLBIs externalBuildTools projectTestOptions sources pkgDesc skippedLibs comp = case comp of
  CLib lib -> library (libTargetName (packageName pkgDesc) (libName lib)) lib
  CExe exe -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (exeName exe)) Executable $ executable exe
  CTest test -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (testName test)) TestSuite $ testSuite test
  CBench bench -> ifNotOnSkippedLib (componentBuildInfo comp) (unUnqualComponentName (benchmarkName bench)) Benchmark $ benchmark bench
  CFLib _ -> skip "foreign library (not supported yet)"
  where
    skip = skipBecause []

    -- Skip a component, with the warnings for the problems that led to it
    -- followed by the skip itself.
    skipBecause problems why =
      mempty{ctWarnings = problems ++ ["cabal buck2: skipping " ++ why ++ " in package " ++ unPackageName (packageName pkgDesc)]}

    -- A component that build-depends on one of *this same package's*
    -- own libraries, when that library was itself skipped (see
    -- 'skippedLibs'), can't be built either - it would emit a rule
    -- whose own @deps@ references a target that was never generated,
    -- which buck2 rejects outright ("Unknown target") at analysis time
    -- for the whole build, not just a warning. cabal-testsuite's own
    -- @test-runtime-deps@ executable (build-depends on cabal-testsuite's
    -- own library) is exactly this case.
    ifNotOnSkippedLib bi targetName kind act =
      case [ln | d <- targetBuildDepends bi, depPkgName d == packageName pkgDesc, ln <- NES.toList (depLibraries d), ln `Set.member` skippedLibs] of
        (ln : _) ->
          skip
            ( kindName kind
                ++ " "
                ++ targetName
                ++ " (depends on "
                ++ libTargetName (packageName pkgDesc) ln
                ++ ", itself skipped)"
            )
        [] -> act

    library targetName lib = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip ("library " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> case resolveModules sources pkgDesc (Just (lbi, clbi)) bi (exposedModules lib ++ otherModules bi) of
        Left problems -> skipBecause problems ("library " ++ targetName ++ " (couldn't resolve all its modules)")
        Right (srcs, srcAutogen) ->
          componentTargets
            (specComponent localIndex externalBuildTools Library targetName bi){scSrcs = srcs}
            (macrosHeader targetName pkgDesc lbi clbi : srcAutogen)
      where
        bi = libBuildInfo lib

    -- An executable, benchmark or test-suite: all the same shape (a main
    -- module over some others), differing only in the spec's kind and test
    -- arguments.
    executableLike kind targetName lbi clbi bi mainSrc otherSrcs testArgs srcAutogen =
      componentTargets
        (specComponent localIndex externalBuildTools kind targetName bi){scMainIs = Just mainSrc, scSrcs = otherSrcs, scTestArgs = testArgs}
        (macrosHeader targetName pkgDesc lbi clbi : srcAutogen)

    -- The shape shared by an executable, an @exitcode-stdio-1.0@
    -- test-suite and benchmark: a @main-is@ file over the @other-modules@.
    mainIsComponent kind targetName bi mainIs extra = case lbiClbiFor pkgDesc componentLBIs comp of
      Nothing -> skip (kindName kind ++ " " ++ targetName ++ " (no LocalBuildInfo found for it in the elaborated build plan)")
      Just (lbi, clbi) -> case (resolveMainIs sources bi mainIs, resolveModules sources pkgDesc (Just (lbi, clbi)) bi (otherModules bi)) of
        (Right mainSrc, Right (otherSrcs, srcAutogen)) ->
          executableLike kind targetName lbi clbi bi (SrcFile mainSrc) otherSrcs (extra lbi) srcAutogen
        (mainRes, othersRes) ->
          skipBecause (problemsOf mainRes ++ problemsOf othersRes) (kindName kind ++ " " ++ targetName ++ " (couldn't resolve all its modules)")

    executable exe = mainIsComponent Executable targetName bi (getSymbolicPath (modulePath exe)) (const [])
      where
        bi = componentBuildInfo (CExe exe)
        targetName = unUnqualComponentName (exeName exe)

    -- A benchmark's own 'BenchmarkExeV10' is exactly 'TestSuiteExeV10's
    -- shape (a version-tagged main-is path over the same 'BuildInfo') -
    -- and unlike a test-suite, @cabal bench@ has no special "run it and
    -- report a testsuite-style result" semantics of its own, just
    -- "build and run this executable" - so this maps onto the same thing
    -- as an executable.
    benchmark bench = case benchmarkInterface bench of
      BenchmarkExeV10 _ver mainIs -> mainIsComponent Benchmark targetName bi (getSymbolicPath mainIs) (const [])
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
      TestSuiteExeV10 _ver mainIs -> mainIsComponent TestSuite targetName bi (getSymbolicPath mainIs) testArgs
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
        Just (lbi, clbi) -> case (resolveModules sources pkgDesc (Just (lbi, clbi)) bi [testModule], resolveModules sources pkgDesc (Just (lbi, clbi)) bi (otherModules bi)) of
          (Right (testModSrc, testModAutogen), Right (otherSrcs, otherAutogen)) ->
            let (stubSrc, stubFile) = detailedTestStub targetName testModule
             in executableLike TestSuite targetName lbi clbi bi stubSrc (testModSrc ++ otherSrcs) (testArgs lbi) (stubFile : testModAutogen ++ otherAutogen)
          (testModRes, othersRes) ->
            skipBecause (problemsOf testModRes ++ problemsOf othersRes) ("test-suite " ++ targetName ++ " (couldn't resolve all its modules)")
      _ ->
        skip
          ( "test-suite "
              ++ targetName
              ++ " (only exitcode-stdio-1.0 and detailed-0.9 test-suites are supported)"
          )
      where
        bi = componentBuildInfo (CTest test)
        targetName = unUnqualComponentName (testName test)
        testArgs lbi = testSuiteArgs pkgDesc lbi test (Map.findWithDefault [] (packageName pkgDesc, componentName comp) projectTestOptions)

    problemsOf = fromLeft []

-- | A package's build spec.
packageSpec :: PackageDescription -> FilePath -> [String] -> [SpecComponent] -> BuildSpec
packageSpec pkgDesc rootRelPkgDir projectGhcOptions components =
  BuildSpec
    { specPackageName = unPackageName (packageName pkgDesc)
    , specPackageDir = rootRelPkgDir
    , specGhcOptions = projectGhcOptions
    , specComponents = components
    }

-- | A component of a build spec, from its 'BuildInfo'. The kind-specific
-- parts (main module, other sources, test arguments) are left empty.
specComponent :: LocalPackageIndex -> Set String -> ComponentKind -> String -> BuildInfo -> SpecComponent
specComponent localIndex externalBuildTools kind name bi =
  SpecComponent
    { scKind = kind
    , scName = name
    , scMainIs = Nothing
    , scSrcs = []
    , scTestArgs = []
    , scGhcOptions = hcOptions GHC bi
    , scCppOptions = cppOptions bi
    , scLanguage = prettyShow <$> defaultLanguage bi
    , scExtensions = map prettyShow (defaultExtensions bi)
    , scExtraLibraries = extraLibs bi
    , scDeps = map depSpec (libraryDeps localIndex bi)
    , scBuildTools = mapMaybe buildToolSpec (ordNub [(pn, exe) | ExeDependency pn exe _ <- buildToolDepends bi])
    , scCSources = map getSymbolicPath (cSources bi)
    , scCxxSources = map getSymbolicPath (cxxSources bi)
    , scCxxOptions = cxxOptions bi
    , scIncludeDirs = map getSymbolicPath (includeDirs bi)
    , scPkgconfig = ordNub [unPkgconfigName n | PkgconfigDependency n _ <- pkgconfigDepends bi]
    }
  where
    depSpec (pn, ln) =
      SpecDep
        { depPackage = unPackageName pn
        , depLibrary = case ln of
            LSubLibName n -> Just (unUnqualComponentName n)
            LMainLibName -> Nothing
        , depDir = fst <$> Map.lookup pn localIndex
        }
    -- A tool that's neither built by this project nor resolved to a real
    -- external binary can't be put on PATH by buck2: dropped, silently -
    -- most build-tool-depends are Setup.hs-time tools nothing needs on PATH.
    buildToolSpec (pn, exe) = case Map.lookup pn localIndex of
      Just (dir, _) -> Just (LocalTool n dir)
      Nothing
        | n `Set.member` externalBuildTools -> Just (ExternalTool n)
        | otherwise -> Nothing
      where
        n = unUnqualComponentName exe

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

-- | This component's @cabal_macros.h@, exactly as real Cabal generates it
-- ahead of a build. Produced unconditionally, like real Cabal: harmless for a
-- component that never enables CPP (the header only matters if\/when cpp
-- actually runs). @cabal_component@ (in buck2\/cabal.bzl's rules) is what
-- wires the component up to include it.
macrosHeader :: String -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> AutogenFile
macrosHeader targetName pkgDesc lbi clbi =
  AutogenFile
    { autogenName = targetName ++ "-cabal-macros"
    , autogenPath = targetName </> "cabal_macros.h"
    , autogenContents = generateCabalMacrosHeader pkgDesc lbi clbi
    }

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

-- | Resolve each module in @hs-source-dirs@ to its real file, trying
-- @.hs@\/@.lhs@\/@.hsc@ (and the other extensions buck2\/haskell.bzl knows
-- how to preprocess) in turn - returning @(moduleName, source)@ pairs for the
-- dict form of @srcs@, which - unlike the plain-list form - is unaffected
-- by @hs-source-dirs@ not matching the BUCK package's own directory. Files
-- are looked up in @sources@ (see 'sourceCandidates'). The package's
-- @Paths_\<pkg\>@ module (if listed) is special-cased: no such file exists
-- anywhere - Cabal's own Setup.hs generates it fresh on every real build -
-- so 'pathsModuleFile' stands one in, which needs this component's real
-- 'LocalBuildInfo'\/'ComponentLocalBuildInfo' (see 'lbiClbiFor'); without
-- one that module fails to resolve, like a missing source file. Also returns
-- the autogen files picked up along the way - in practice just the
-- @Paths_\<pkg\>@ one, at most once, if that module was among @mods@.
--
-- 'Left' (with a description of each module that couldn't be resolved) if
-- *any* module couldn't be - the caller skips the whole component in that
-- case, rather than emitting a rule that references a source file that
-- doesn't exist: buck2 doesn't merely warn about that, it fails outright
-- while evaluating the @BUCK@ file, which (since a @buck2 build //...@
-- evaluates every @BUCK@ file up front) would otherwise take the *entire*
-- build down over one unresolvable module in one component of one package.
resolveModules :: Set FilePath -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> BuildInfo -> [ModuleName.ModuleName] -> Either [String] ([(String, Src)], [AutogenFile])
resolveModules sources pkgDesc mlbiClbi bi mods =
  case partitionEithers (map (resolveOne sources pkgDesc mlbiClbi bi) mods) of
    ([], triples) -> Right ([(n, v) | (n, v, _) <- triples], concat [fs | (_, _, fs) <- triples])
    (problems, _) -> Left problems

resolveOne :: Set FilePath -> PackageDescription -> Maybe (LocalBuildInfo, ComponentLocalBuildInfo) -> BuildInfo -> ModuleName.ModuleName -> Either String (String, Src, [AutogenFile])
resolveOne sources pkgDesc mlbiClbi bi m
  | m == autogenPathsModuleName pkgDesc = case mlbiClbi of
      Nothing ->
        Left $ "cabal buck2: couldn't generate " ++ prettyShow m ++ " (no LocalBuildInfo available for this component)"
      Just (lbi, clbi) ->
        let file = pathsModuleFile pkgDesc lbi clbi m
         in Right (prettyShow m, SrcAutogen (autogenName file), [file])
  | otherwise = case firstExisting sources (moduleCandidates bi m) of
      Just real -> Right (prettyShow m, SrcFile real, [])
      Nothing ->
        Left $
          "cabal buck2: couldn't find a source file for module "
            ++ prettyShow m
            ++ " under "
            ++ intercalate ", " (sourceDirs bi)

-- | Cabal's own Setup.hs generates a @Paths_\<pkg\>@ module fresh at
-- configure\/build time (giving @version@\/@getDataFileName@\/etc) - no
-- real source file for it exists anywhere to find. Generated here from
-- real Cabal's own 'generatePathsModule' (given this component's real
-- 'LocalBuildInfo'\/'ComponentLocalBuildInfo' - see 'lbiClbiFor'), so
-- install-dir\/relocatability logic matches a plain @cabal build@ exactly.
-- It is referenced from @srcs@ by its @export_file()@ target rather than a
-- path: @cabal-buck2\/autogen\/@ has its own @BUCK@ file, so it is a
-- different buck2 package from the one the sources are in, and a file living
-- there can no longer be named by a package-relative path.
pathsModuleFile :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> ModuleName.ModuleName -> AutogenFile
pathsModuleFile pkgDesc lbi clbi m =
  AutogenFile
    { autogenName = ModuleName.toFilePath m
    , autogenPath = ModuleName.toFilePath m <.> "hs"
    , autogenContents = generatePathsModule pkgDesc lbi clbi
    }

-- | A @detailed-0.9@ test-suite's own stub @Main@ - see 'testSuite's own
-- haddock for why this is a from-scratch driver over
-- @Distribution.TestSuite@'s public API, not real Cabal's own
-- @Setup.hs@-generated one (@Distribution.Simple.Test.LibV09.stubMain@,
-- which expects a handshake over stdin buck2 has no way to provide).
-- Returned as the test-suite's @main_is@ source, along with the file,
-- referenced by target for the same reason as 'pathsModuleFile'.
detailedTestStub :: String -> ModuleName.ModuleName -> (Src, AutogenFile)
detailedTestStub targetName testModule = (SrcAutogen name, file)
  where
    name = targetName ++ "-stub-main"
    file = AutogenFile{autogenName = name, autogenPath = targetName </> "Main.hs", autogenContents = contents}
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

-- | The files a module's source could be, in order of preference: in each
-- of the @hs-source-dirs@ in turn, with each extension buck2/haskell.bzl's
-- own srcs-resolution (@_resolve_src@) knows to run through
-- hsc2hs()\/alex()\/happy().
moduleCandidates :: BuildInfo -> ModuleName.ModuleName -> [FilePath]
moduleCandidates bi m = [dir </> ModuleName.toFilePath m <.> ext | dir <- sourceDirs bi, ext <- ["hs", "lhs", "hsc", "x", "y"]]

mainIsCandidates :: BuildInfo -> FilePath -> [FilePath]
mainIsCandidates bi mainIs = [dir </> mainIs | dir <- sourceDirs bi]

firstExisting :: Set FilePath -> [FilePath] -> Maybe FilePath
firstExisting sources = find (`Set.member` sources)

-- | 'Left' if the main-is file couldn't be found - see 'resolveModules'
-- for why the caller must skip the whole component rather than emit a
-- rule pointing at a nonexistent file.
resolveMainIs :: Set FilePath -> BuildInfo -> FilePath -> Either [String] FilePath
resolveMainIs sources bi mainIs =
  maybe (Left ["cabal buck2: couldn't find main-is file " ++ mainIs ++ " under " ++ intercalate ", " (sourceDirs bi)]) Right $
    firstExisting sources (mainIsCandidates bi mainIs)

-- | Every path (relative to the package's directory) that
-- 'generatePackageTargets' may look for a component's source in. The
-- generator does no IO, so callers check which of these exist and pass those
-- in.
sourceCandidates :: PackageDescription -> [FilePath]
sourceCandidates pkgDesc = ordNub (concatMap candidates (pkgBuildableComponents pkgDesc))
  where
    candidates comp = case comp of
      CLib lib -> modules bi (exposedModules lib ++ otherModules bi)
      CExe exe -> mainIs bi (getSymbolicPath (modulePath exe)) ++ modules bi (otherModules bi)
      CTest test -> case testInterface test of
        TestSuiteExeV10 _ main -> mainIs bi (getSymbolicPath main) ++ modules bi (otherModules bi)
        TestSuiteLibV09 _ testModule -> modules bi (testModule : otherModules bi)
        _ -> []
      CBench bench -> case benchmarkInterface bench of
        BenchmarkExeV10 _ main -> mainIs bi (getSymbolicPath main) ++ modules bi (otherModules bi)
        _ -> []
      CFLib _ -> []
      where
        bi = componentBuildInfo comp
    modules bi = concatMap (moduleCandidates bi)
    mainIs = mainIsCandidates

sourceDirs :: BuildInfo -> [FilePath]
sourceDirs bi = case map getSymbolicPath (hsSourceDirs bi) of
  [] -> ["."]
  ds -> ds

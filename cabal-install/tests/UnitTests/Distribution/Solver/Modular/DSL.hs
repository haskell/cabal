{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | DSL for testing the modular solver
module UnitTests.Distribution.Solver.Modular.DSL
  ( ExampleDependency (..)
  , Dependencies (..)
  , ExSubLib (..)
  , ExTest (..)
  , ExExe (..)
  , ExConstraint (..)
  , ExPreference (..)
  , ExampleDb
  , ExampleVersionRange
  , ExamplePkgVersion
  , ExamplePkgName
  , ExampleFlagName
  , ExFlag (..)
  , ExampleAvailable (..)
  , ExampleInstalled (..)
  , ExampleQualifier (..)
  , ExampleVar (..)
  , EnableAllTests (..)
  , dependencies
  , publicDependencies
  , unbuildableDependencies
  , exAv
  , exAvNoLibrary
  , exInst
  , exSubLib
  , exTest
  , exExe
  , exFlagged
  , exResolve
  , extractInstallPlan
  , declareFlags
  , withSubLibrary
  , withSubLibraries
  , withSetupDeps
  , withTest
  , withTests
  , withExe
  , withExes
  , runProgress
  , mkSimpleVersion
  , mkVersionRange
  ) where

import Distribution.Solver.Compat.Prelude
import Distribution.Utils.Generic
import Prelude ()

-- base
import Control.Arrow (second)
import qualified Data.Map as Map
import qualified Distribution.Compat.NonEmptySet as NonEmptySet

-- Cabal
import qualified Distribution.CabalSpecVersion as C
import qualified Distribution.Compiler as C
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.License (License (..))
import qualified Distribution.ModuleName as Module
import qualified Distribution.Package as C hiding
  ( HasUnitId (..)
  )
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Check as C
import qualified Distribution.Simple.PackageIndex as C.PackageIndex
import Distribution.Simple.Setup (BooleanFlag (..))
import qualified Distribution.System as C
import Distribution.Text (display)
import qualified Distribution.Utils.Path as C
import qualified Distribution.Verbosity as C
import qualified Distribution.Version as C
import Language.Haskell.Extension (Extension (..), Language (..))

-- cabal-install
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
import qualified Distribution.Client.SolverInstallPlan as CI.SolverInstallPlan
import Distribution.Client.Types

import Distribution.Solver.Types.ComponentDeps (ComponentDeps)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.Flag
import Distribution.Solver.Types.LabeledPackageConstraint
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackageIndex as CI.PackageIndex
import qualified Distribution.Solver.Types.PackagePath as P
import qualified Distribution.Solver.Types.PkgConfigDb as PC
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.SolverPackage
import Distribution.Solver.Types.SourcePackage
import Distribution.Solver.Types.Variable

{-------------------------------------------------------------------------------
  Example package database DSL

  In order to be able to set simple examples up quickly, we define a very
  simple version of the package database here explicitly designed for use in
  tests.

  The design of `ExampleDb` takes the perspective of the solver, not the
  perspective of the package DB. This makes it easier to set up tests for
  various parts of the solver, but makes the mapping somewhat awkward,  because
  it means we first map from "solver perspective" `ExampleDb` to the package
  database format, and then the modular solver internally in `IndexConversion`
  maps this back to the solver specific data structures.

  IMPLEMENTATION NOTES
  --------------------

  TODO: Perhaps these should be made comments of the corresponding data type
  definitions. For now these are just my own conclusions and may be wrong.

  * The difference between `GenericPackageDescription` and `PackageDescription`
    is that `PackageDescription` describes a particular _configuration_ of a
    package (for instance, see documentation for `checkPackage`). A
    `GenericPackageDescription` can be turned into a `PackageDescription` in
    two ways:

      a. `finalizePD` does the proper translation, by taking
         into account the platform, available dependencies, etc. and picks a
         flag assignment (or gives an error if no flag assignment can be found)
      b. `flattenPackageDescription` ignores flag assignment and just joins all
         components together.

    The slightly odd thing is that a `GenericPackageDescription` contains a
    `PackageDescription` as a field; both of the above functions do the same
    thing: they take the embedded `PackageDescription` as a basis for the result
    value, but override `library`, `executables`, `testSuites`, `benchmarks`
    and `buildDepends`.
  * The `condTreeComponents` fields of a `CondTree` is a list of triples
    `(condition, then-branch, else-branch)`, where the `else-branch` is
    optional.
-------------------------------------------------------------------------------}

type ExamplePkgName = String
type ExamplePkgVersion = Int
type ExamplePkgHash = String -- for example "installed" packages
type ExampleFlagName = String
type ExampleSubLibName = String
type ExampleTestName = String
type ExampleExeName = String
type ExampleVersionRange = C.VersionRange

data Dependencies = Dependencies
  { depsVisibility :: C.LibraryVisibility
  , depsIsBuildable :: Bool
  , depsExampleDependencies :: [ExampleDependency]
  }
  deriving (Show)

instance Semigroup Dependencies where
  deps1 <> deps2 =
    Dependencies
      { depsVisibility = depsVisibility deps1 <> depsVisibility deps2
      , depsIsBuildable = depsIsBuildable deps1 && depsIsBuildable deps2
      , depsExampleDependencies = depsExampleDependencies deps1 ++ depsExampleDependencies deps2
      }

instance Monoid Dependencies where
  mempty =
    Dependencies
      { depsVisibility = mempty
      , depsIsBuildable = True
      , depsExampleDependencies = []
      }
  mappend = (<>)

dependencies :: [ExampleDependency] -> Dependencies
dependencies deps = mempty{depsExampleDependencies = deps}

publicDependencies :: Dependencies
publicDependencies = mempty{depsVisibility = C.LibraryVisibilityPublic}

unbuildableDependencies :: Dependencies
unbuildableDependencies = mempty{depsIsBuildable = False}

data ExampleDependency
  = -- | Simple dependency on any version
    ExAny ExamplePkgName
  | -- | Simple dependency on a fixed version
    ExFix ExamplePkgName ExamplePkgVersion
  | -- | Simple dependency on a range of versions, with an inclusive lower bound
    -- and an exclusive upper bound.
    ExRange ExamplePkgName ExamplePkgVersion ExamplePkgVersion
  | -- | Sub-library dependency
    ExSubLibAny ExamplePkgName ExampleSubLibName
  | -- | Sub-library dependency on a fixed version
    ExSubLibFix ExamplePkgName ExampleSubLibName ExamplePkgVersion
  | -- | Build-tool-depends dependency
    ExBuildToolAny ExamplePkgName ExampleExeName
  | -- | Build-tool-depends dependency on a fixed version
    ExBuildToolFix ExamplePkgName ExampleExeName ExamplePkgVersion
  | -- | Legacy build-tools dependency
    ExLegacyBuildToolAny ExamplePkgName
  | -- | Legacy build-tools dependency on a fixed version
    ExLegacyBuildToolFix ExamplePkgName ExamplePkgVersion
  | -- | Dependencies indexed by a flag
    ExFlagged ExampleFlagName Dependencies Dependencies
  | -- | Dependency on a language extension
    ExExt Extension
  | -- | Dependency on a language version
    ExLang Language
  | -- | Dependency on a pkg-config package
    ExPkg (ExamplePkgName, ExamplePkgVersion)
  deriving (Show)

-- | Simplified version of D.Types.GenericPackageDescription.Flag for use in
-- example source packages.
data ExFlag = ExFlag
  { exFlagName :: ExampleFlagName
  , exFlagDefault :: Bool
  , exFlagType :: FlagType
  }
  deriving (Show)

data ExSubLib = ExSubLib ExampleSubLibName Dependencies

data ExTest = ExTest ExampleTestName Dependencies

data ExExe = ExExe ExampleExeName Dependencies

exSubLib :: ExampleSubLibName -> [ExampleDependency] -> ExSubLib
exSubLib name deps = ExSubLib name (dependencies deps)

exTest :: ExampleTestName -> [ExampleDependency] -> ExTest
exTest name deps = ExTest name (dependencies deps)

exExe :: ExampleExeName -> [ExampleDependency] -> ExExe
exExe name deps = ExExe name (dependencies deps)

exFlagged
  :: ExampleFlagName
  -> [ExampleDependency]
  -> [ExampleDependency]
  -> ExampleDependency
exFlagged n t e = ExFlagged n (dependencies t) (dependencies e)

data ExConstraint
  = ExVersionConstraint ConstraintScope ExampleVersionRange
  | ExFlagConstraint ConstraintScope ExampleFlagName Bool
  | ExStanzaConstraint ConstraintScope [OptionalStanza]
  deriving (Show)

data ExPreference
  = ExPkgPref ExamplePkgName ExampleVersionRange
  | ExStanzaPref ExamplePkgName [OptionalStanza]
  deriving (Show)

data ExampleAvailable = ExAv
  { exAvName :: ExamplePkgName
  , exAvVersion :: ExamplePkgVersion
  , exAvDeps :: ComponentDeps Dependencies
  , -- Setting flags here is only necessary to override the default values of
    -- the fields in C.Flag.
    exAvFlags :: [ExFlag]
  }
  deriving (Show)

data ExampleVar
  = P ExampleQualifier ExamplePkgName
  | F ExampleQualifier ExamplePkgName ExampleFlagName
  | S ExampleQualifier ExamplePkgName OptionalStanza

data ExampleQualifier
  = QualNone
  | QualIndep ExamplePkgName
  | QualSetup ExamplePkgName
  | -- The two package names are the build target and the package containing the
    -- setup script.
    QualIndepSetup ExamplePkgName ExamplePkgName
  | -- The two package names are the package depending on the exe and the
    -- package containing the exe.
    QualExe ExamplePkgName ExamplePkgName

-- | Whether to enable tests in all packages in a test case.
newtype EnableAllTests = EnableAllTests Bool
  deriving (BooleanFlag)

-- | Constructs an 'ExampleAvailable' package for the 'ExampleDb',
-- given:
--
--      1. The name 'ExamplePkgName' of the available package,
--      2. The version 'ExamplePkgVersion' available
--      3. The list of dependency constraints ('ExampleDependency')
--         for this package's library component.  'ExampleDependency'
--         provides a number of pre-canned dependency types to look at.
exAv
  :: ExamplePkgName
  -> ExamplePkgVersion
  -> [ExampleDependency]
  -> ExampleAvailable
exAv n v ds = (exAvNoLibrary n v){exAvDeps = CD.fromLibraryDeps (dependencies ds)}

-- | Constructs an 'ExampleAvailable' package without a default library
-- component.
exAvNoLibrary :: ExamplePkgName -> ExamplePkgVersion -> ExampleAvailable
exAvNoLibrary n v =
  ExAv
    { exAvName = n
    , exAvVersion = v
    , exAvDeps = CD.empty
    , exAvFlags = []
    }

-- | Override the default settings (e.g., manual vs. automatic) for a subset of
-- a package's flags.
declareFlags :: [ExFlag] -> ExampleAvailable -> ExampleAvailable
declareFlags flags ex =
  ex
    { exAvFlags = flags
    }

withSubLibrary :: ExampleAvailable -> ExSubLib -> ExampleAvailable
withSubLibrary ex lib = withSubLibraries ex [lib]

withSubLibraries :: ExampleAvailable -> [ExSubLib] -> ExampleAvailable
withSubLibraries ex libs =
  let subLibCDs =
        CD.fromList
          [ (CD.ComponentSubLib $ C.mkUnqualComponentName name, deps)
          | ExSubLib name deps <- libs
          ]
   in ex{exAvDeps = exAvDeps ex <> subLibCDs}

withSetupDeps :: ExampleAvailable -> [ExampleDependency] -> ExampleAvailable
withSetupDeps ex setupDeps =
  ex
    { exAvDeps = exAvDeps ex <> CD.fromSetupDeps (dependencies setupDeps)
    }

withTest :: ExampleAvailable -> ExTest -> ExampleAvailable
withTest ex test = withTests ex [test]

withTests :: ExampleAvailable -> [ExTest] -> ExampleAvailable
withTests ex tests =
  let testCDs =
        CD.fromList
          [ (CD.ComponentTest $ C.mkUnqualComponentName name, deps)
          | ExTest name deps <- tests
          ]
   in ex{exAvDeps = exAvDeps ex <> testCDs}

withExe :: ExampleAvailable -> ExExe -> ExampleAvailable
withExe ex exe = withExes ex [exe]

withExes :: ExampleAvailable -> [ExExe] -> ExampleAvailable
withExes ex exes =
  let exeCDs =
        CD.fromList
          [ (CD.ComponentExe $ C.mkUnqualComponentName name, deps)
          | ExExe name deps <- exes
          ]
   in ex{exAvDeps = exAvDeps ex <> exeCDs}

-- | An installed package in 'ExampleDb'; construct me with 'exInst'.
data ExampleInstalled = ExInst
  { exInstName :: ExamplePkgName
  , exInstVersion :: ExamplePkgVersion
  , exInstHash :: ExamplePkgHash
  , exInstBuildAgainst :: [ExamplePkgHash]
  }
  deriving (Show)

-- | Constructs an example installed package given:
--
--      1. The name of the package 'ExamplePkgName', i.e., 'String'
--      2. The version of the package 'ExamplePkgVersion', i.e., 'Int'
--      3. The IPID for the package 'ExamplePkgHash', i.e., 'String'
--         (just some unique identifier for the package.)
--      4. The 'ExampleInstalled' packages which this package was
--         compiled against.)
exInst
  :: ExamplePkgName
  -> ExamplePkgVersion
  -> ExamplePkgHash
  -> [ExampleInstalled]
  -> ExampleInstalled
exInst pn v hash deps = ExInst pn v hash (map exInstHash deps)

-- | An example package database is a list of installed packages
-- 'ExampleInstalled' and available packages 'ExampleAvailable'.
-- Generally, you want to use 'exInst' and 'exAv' to construct
-- these packages.
type ExampleDb = [Either ExampleInstalled ExampleAvailable]

type DependencyTree a = C.CondTree C.ConfVar [C.Dependency] a

type DependencyComponent a = C.CondBranch C.ConfVar [C.Dependency] a

exDbPkgs :: ExampleDb -> [ExamplePkgName]
exDbPkgs = map (either exInstName exAvName)

exAvSrcPkg :: ExampleAvailable -> UnresolvedSourcePackage
exAvSrcPkg ex =
  let pkgId = exAvPkgId ex

      flags :: [C.PackageFlag]
      flags =
        let declaredFlags :: Map ExampleFlagName C.PackageFlag
            declaredFlags =
              Map.fromListWith
                (\f1 f2 -> error $ "duplicate flag declarations: " ++ show [f1, f2])
                [(exFlagName flag, mkFlag flag) | flag <- exAvFlags ex]

            usedFlags :: Map ExampleFlagName C.PackageFlag
            usedFlags = Map.fromList [(fn, mkDefaultFlag fn) | fn <- names]
              where
                names = extractFlags $ CD.flatDeps (exAvDeps ex)
         in -- 'declaredFlags' overrides 'usedFlags' to give flags non-default settings:
            Map.elems $ declaredFlags `Map.union` usedFlags

      subLibraries = [(name, deps) | (CD.ComponentSubLib name, deps) <- CD.toList (exAvDeps ex)]
      foreignLibraries = [(name, deps) | (CD.ComponentFLib name, deps) <- CD.toList (exAvDeps ex)]
      testSuites = [(name, deps) | (CD.ComponentTest name, deps) <- CD.toList (exAvDeps ex)]
      benchmarks = [(name, deps) | (CD.ComponentBench name, deps) <- CD.toList (exAvDeps ex)]
      executables = [(name, deps) | (CD.ComponentExe name, deps) <- CD.toList (exAvDeps ex)]
      setup = case depsExampleDependencies $ CD.setupDeps (exAvDeps ex) of
        [] -> Nothing
        deps ->
          Just
            C.SetupBuildInfo
              { C.setupDepends = mkSetupDeps deps
              , C.defaultSetupDepends = False
              }
      package =
        SourcePackage
          { srcpkgPackageId = pkgId
          , srcpkgSource = LocalTarballPackage "<<path>>"
          , srcpkgDescrOverride = Nothing
          , srcpkgDescription =
              C.GenericPackageDescription
                { C.packageDescription =
                    C.emptyPackageDescription
                      { C.package = pkgId
                      , C.setupBuildInfo = setup
                      , C.licenseRaw = Right BSD3
                      , C.buildTypeRaw =
                          if isNothing setup
                            then Just C.Simple
                            else Just C.Custom
                      , C.category = "category"
                      , C.maintainer = "maintainer"
                      , C.description = "description"
                      , C.synopsis = "synopsis"
                      , C.licenseFiles = [C.unsafeMakeSymbolicPath "LICENSE"]
                      , -- Version 2.0 is required for internal libraries.
                        C.specVersion = C.CabalSpecV2_0
                      }
                , C.gpdScannedVersion = Nothing
                , C.genPackageFlags = flags
                , C.condLibrary =
                    let mkLib v bi = mempty{C.libVisibility = v, C.libBuildInfo = bi}
                        -- Avoid using the Monoid instance for [a] when getting
                        -- the library dependencies, to allow for the possibility
                        -- that the package doesn't have a library:
                        libDeps = lookup CD.ComponentLib (CD.toList (exAvDeps ex))
                     in mkTopLevelCondTree defaultLib mkLib <$> libDeps
                , C.condSubLibraries =
                    let mkTree = mkTopLevelCondTree defaultSubLib mkLib
                        mkLib v bi = mempty{C.libVisibility = v, C.libBuildInfo = bi}
                     in map (second mkTree) subLibraries
                , C.condForeignLibs =
                    let mkTree = mkTopLevelCondTree (mkLib defaultTopLevelBuildInfo) (const mkLib)
                        mkLib bi = mempty{C.foreignLibBuildInfo = bi}
                     in map (second mkTree) foreignLibraries
                , C.condExecutables =
                    let mkTree = mkTopLevelCondTree defaultExe (const mkExe)
                        mkExe bi = mempty{C.buildInfo = bi}
                     in map (second mkTree) executables
                , C.condTestSuites =
                    let mkTree = mkTopLevelCondTree defaultTest (const mkTest)
                        mkTest bi = mempty{C.testBuildInfo = bi}
                     in map (second mkTree) testSuites
                , C.condBenchmarks =
                    let mkTree = mkTopLevelCondTree defaultBenchmark (const mkBench)
                        mkBench bi = mempty{C.benchmarkBuildInfo = bi}
                     in map (second mkTree) benchmarks
                }
          }
      pkgCheckErrors =
        -- We ignore unknown extensions/languages warnings because
        -- some there are some unit tests test in which the solver allows
        -- unknown extensions/languages when the compiler supports them.
        -- Furthermore we ignore missing upper bound warnings because
        -- they are not related to this test suite, and are tested
        -- with golden tests.
        let checks = C.checkPackage (srcpkgDescription package) Nothing
         in filter (\x -> not (isMissingUpperBound x) && not (isUnknownLangExt x)) checks
   in if null pkgCheckErrors
        then package
        else
          error $
            "invalid GenericPackageDescription for package "
              ++ display pkgId
              ++ ": "
              ++ show pkgCheckErrors
  where
    defaultTopLevelBuildInfo :: C.BuildInfo
    defaultTopLevelBuildInfo = mempty{C.defaultLanguage = Just Haskell98}

    defaultLib :: C.Library
    defaultLib =
      mempty
        { C.libBuildInfo = defaultTopLevelBuildInfo
        , C.exposedModules = [Module.fromString "Module"]
        , C.libVisibility = C.LibraryVisibilityPublic
        }

    defaultSubLib :: C.Library
    defaultSubLib =
      mempty
        { C.libBuildInfo = defaultTopLevelBuildInfo
        , C.exposedModules = [Module.fromString "Module"]
        }

    defaultExe :: C.Executable
    defaultExe =
      mempty
        { C.buildInfo = defaultTopLevelBuildInfo
        , C.modulePath = "Main.hs"
        }

    defaultTest :: C.TestSuite
    defaultTest =
      mempty
        { C.testBuildInfo = defaultTopLevelBuildInfo
        , C.testInterface = C.TestSuiteExeV10 (C.mkVersion [1, 0]) "Test.hs"
        }

    defaultBenchmark :: C.Benchmark
    defaultBenchmark =
      mempty
        { C.benchmarkBuildInfo = defaultTopLevelBuildInfo
        , C.benchmarkInterface = C.BenchmarkExeV10 (C.mkVersion [1, 0]) "Benchmark.hs"
        }

    -- Split the set of dependencies into the set of dependencies of the library,
    -- the dependencies of the test suites and extensions.
    splitTopLevel
      :: [ExampleDependency]
      -> ( [ExampleDependency]
         , [Extension]
         , Maybe Language
         , [(ExamplePkgName, ExamplePkgVersion)] -- pkg-config
         , [(ExamplePkgName, ExampleExeName, C.VersionRange)] -- build tools
         , [(ExamplePkgName, C.VersionRange)] -- legacy build tools
         )
    splitTopLevel [] =
      ([], [], Nothing, [], [], [])
    splitTopLevel (ExBuildToolAny p e : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (other, exts, lang, pcpkgs, (p, e, C.anyVersion) : exes, legacyExes)
    splitTopLevel (ExBuildToolFix p e v : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (other, exts, lang, pcpkgs, (p, e, C.thisVersion (mkSimpleVersion v)) : exes, legacyExes)
    splitTopLevel (ExLegacyBuildToolAny p : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (other, exts, lang, pcpkgs, exes, (p, C.anyVersion) : legacyExes)
    splitTopLevel (ExLegacyBuildToolFix p v : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (other, exts, lang, pcpkgs, exes, (p, C.thisVersion (mkSimpleVersion v)) : legacyExes)
    splitTopLevel (ExExt ext : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (other, ext : exts, lang, pcpkgs, exes, legacyExes)
    splitTopLevel (ExLang lang : deps) =
      case splitTopLevel deps of
        (other, exts, Nothing, pcpkgs, exes, legacyExes) -> (other, exts, Just lang, pcpkgs, exes, legacyExes)
        _ -> error "Only 1 Language dependency is supported"
    splitTopLevel (ExPkg pkg : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (other, exts, lang, pkg : pcpkgs, exes, legacyExes)
    splitTopLevel (dep : deps) =
      let (other, exts, lang, pcpkgs, exes, legacyExes) = splitTopLevel deps
       in (dep : other, exts, lang, pcpkgs, exes, legacyExes)

    -- Extract the total set of flags used
    extractFlags :: Dependencies -> [ExampleFlagName]
    extractFlags deps = concatMap go (depsExampleDependencies deps)
      where
        go :: ExampleDependency -> [ExampleFlagName]
        go (ExAny _) = []
        go (ExFix _ _) = []
        go (ExRange _ _ _) = []
        go (ExSubLibAny _ _) = []
        go (ExSubLibFix _ _ _) = []
        go (ExBuildToolAny _ _) = []
        go (ExBuildToolFix _ _ _) = []
        go (ExLegacyBuildToolAny _) = []
        go (ExLegacyBuildToolFix _ _) = []
        go (ExFlagged f a b) = f : extractFlags a ++ extractFlags b
        go (ExExt _) = []
        go (ExLang _) = []
        go (ExPkg _) = []

    -- Convert 'Dependencies' into a tree of a specific component type, using
    -- the given top level component and function for creating a component at
    -- any level.
    mkTopLevelCondTree
      :: forall a
       . Semigroup a
      => a
      -> (C.LibraryVisibility -> C.BuildInfo -> a)
      -> Dependencies
      -> DependencyTree a
    mkTopLevelCondTree defaultTopLevel mkComponent deps =
      let condNode = mkCondTree mkComponent deps
       in condNode{C.condTreeData = defaultTopLevel <> C.condTreeData condNode}

    -- Convert 'Dependencies' into a tree of a specific component type, using
    -- the given function to generate each component.
    mkCondTree :: (C.LibraryVisibility -> C.BuildInfo -> a) -> Dependencies -> DependencyTree a
    mkCondTree mkComponent deps =
      let (libraryDeps, exts, mlang, pcpkgs, buildTools, legacyBuildTools) = splitTopLevel (depsExampleDependencies deps)
          (directDeps, flaggedDeps) = splitDeps libraryDeps
          component = mkComponent (depsVisibility deps) bi
          bi =
            mempty
              { C.otherExtensions = exts
              , C.defaultLanguage = mlang
              , C.buildToolDepends =
                  [ C.ExeDependency (C.mkPackageName p) (C.mkUnqualComponentName e) vr
                  | (p, e, vr) <- buildTools
                  ]
              , C.buildTools =
                  [ C.LegacyExeDependency n vr
                  | (n, vr) <- legacyBuildTools
                  ]
              , C.pkgconfigDepends =
                  [ C.PkgconfigDependency n' v'
                  | (n, v) <- pcpkgs
                  , let n' = C.mkPkgconfigName n
                  , let v' = C.PcThisVersion (mkSimplePkgconfigVersion v)
                  ]
              , C.buildable = depsIsBuildable deps
              }
       in C.CondNode
            { C.condTreeData = component
            , -- TODO: Arguably, build-tools dependencies should also
              -- effect constraints on conditional tree. But no way to
              -- distinguish between them
              C.condTreeConstraints = map mkDirect directDeps
            , C.condTreeComponents = map (mkFlagged mkComponent) flaggedDeps
            }

    mkDirect :: (ExamplePkgName, C.LibraryName, C.VersionRange) -> C.Dependency
    mkDirect (dep, name, vr) = C.Dependency (C.mkPackageName dep) vr (NonEmptySet.singleton name)

    mkFlagged
      :: (C.LibraryVisibility -> C.BuildInfo -> a)
      -> (ExampleFlagName, Dependencies, Dependencies)
      -> DependencyComponent a
    mkFlagged mkComponent (f, a, b) =
      C.CondBranch
        (C.Var (C.PackageFlag (C.mkFlagName f)))
        (mkCondTree mkComponent a)
        (Just (mkCondTree mkComponent b))

    -- Split a set of dependencies into direct dependencies and flagged
    -- dependencies. A direct dependency is a tuple of the name of package and
    -- its version range meant to be converted to a 'C.Dependency' with
    -- 'mkDirect' for example. A flagged dependency is the set of dependencies
    -- guarded by a flag.
    splitDeps
      :: [ExampleDependency]
      -> ( [(ExamplePkgName, C.LibraryName, C.VersionRange)]
         , [(ExampleFlagName, Dependencies, Dependencies)]
         )
    splitDeps [] =
      ([], [])
    splitDeps (ExAny p : deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
       in ((p, C.LMainLibName, C.anyVersion) : directDeps, flaggedDeps)
    splitDeps (ExFix p v : deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
       in ((p, C.LMainLibName, C.thisVersion $ mkSimpleVersion v) : directDeps, flaggedDeps)
    splitDeps (ExRange p v1 v2 : deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
       in ((p, C.LMainLibName, mkVersionRange v1 v2) : directDeps, flaggedDeps)
    splitDeps (ExSubLibAny p lib : deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
       in ((p, C.LSubLibName (C.mkUnqualComponentName lib), C.anyVersion) : directDeps, flaggedDeps)
    splitDeps (ExSubLibFix p lib v : deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
       in ((p, C.LSubLibName (C.mkUnqualComponentName lib), C.thisVersion $ mkSimpleVersion v) : directDeps, flaggedDeps)
    splitDeps (ExFlagged f a b : deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
       in (directDeps, (f, a, b) : flaggedDeps)
    splitDeps (dep : _) = error $ "Unexpected dependency: " ++ show dep

    -- custom-setup only supports simple dependencies
    mkSetupDeps :: [ExampleDependency] -> [C.Dependency]
    mkSetupDeps deps =
      case splitDeps deps of
        (directDeps, []) -> map mkDirect directDeps
        _ -> error "mkSetupDeps: custom setup has non-simple deps"

    -- Check for `UnknownLanguages` and `UnknownExtensions`. See
    isUnknownLangExt :: C.PackageCheck -> Bool
    isUnknownLangExt pc = case C.explanation pc of
      C.UnknownExtensions{} -> True
      C.UnknownLanguages{} -> True
      _ -> False
    isMissingUpperBound :: C.PackageCheck -> Bool
    isMissingUpperBound pc = case C.explanation pc of
      C.MissingUpperBounds{} -> True
      _ -> False

mkSimpleVersion :: ExamplePkgVersion -> C.Version
mkSimpleVersion n = C.mkVersion [n, 0, 0]

mkSimplePkgconfigVersion :: ExamplePkgVersion -> C.PkgconfigVersion
mkSimplePkgconfigVersion = C.versionToPkgconfigVersion . mkSimpleVersion

mkVersionRange :: ExamplePkgVersion -> ExamplePkgVersion -> C.VersionRange
mkVersionRange v1 v2 =
  C.intersectVersionRanges
    (C.orLaterVersion $ mkSimpleVersion v1)
    (C.earlierVersion $ mkSimpleVersion v2)

mkFlag :: ExFlag -> C.PackageFlag
mkFlag flag =
  C.MkPackageFlag
    { C.flagName = C.mkFlagName $ exFlagName flag
    , C.flagDescription = ""
    , C.flagDefault = exFlagDefault flag
    , C.flagManual =
        case exFlagType flag of
          Manual -> True
          Automatic -> False
    }

mkDefaultFlag :: ExampleFlagName -> C.PackageFlag
mkDefaultFlag flag =
  C.MkPackageFlag
    { C.flagName = C.mkFlagName flag
    , C.flagDescription = ""
    , C.flagDefault = True
    , C.flagManual = False
    }

exAvPkgId :: ExampleAvailable -> C.PackageIdentifier
exAvPkgId ex =
  C.PackageIdentifier
    { pkgName = C.mkPackageName (exAvName ex)
    , pkgVersion = C.mkVersion [exAvVersion ex, 0, 0]
    }

exInstInfo :: ExampleInstalled -> IPI.InstalledPackageInfo
exInstInfo ex =
  IPI.emptyInstalledPackageInfo
    { IPI.installedUnitId = C.mkUnitId (exInstHash ex)
    , IPI.sourcePackageId = exInstPkgId ex
    , IPI.depends = map C.mkUnitId (exInstBuildAgainst ex)
    }

exInstPkgId :: ExampleInstalled -> C.PackageIdentifier
exInstPkgId ex =
  C.PackageIdentifier
    { pkgName = C.mkPackageName (exInstName ex)
    , pkgVersion = C.mkVersion [exInstVersion ex, 0, 0]
    }

exAvIdx :: [ExampleAvailable] -> CI.PackageIndex.PackageIndex UnresolvedSourcePackage
exAvIdx = CI.PackageIndex.fromList . map exAvSrcPkg

exInstIdx :: [ExampleInstalled] -> C.PackageIndex.InstalledPackageIndex
exInstIdx = C.PackageIndex.fromList . map exInstInfo

exResolve
  :: ExampleDb
  -- List of extensions supported by the compiler, or Nothing if unknown.
  -> Maybe [Extension]
  -- List of languages supported by the compiler, or Nothing if unknown.
  -> Maybe [Language]
  -> PC.PkgConfigDb
  -> [ExamplePkgName]
  -> Maybe Int
  -> CountConflicts
  -> FineGrainedConflicts
  -> MinimizeConflictSet
  -> IndependentGoals
  -> PreferOldest
  -> ReorderGoals
  -> AllowBootLibInstalls
  -> OnlyConstrained
  -> EnableBackjumping
  -> SolveExecutables
  -> Maybe (Variable P.QPN -> Variable P.QPN -> Ordering)
  -> [ExConstraint]
  -> [ExPreference]
  -> C.Verbosity
  -> EnableAllTests
  -> Progress String String CI.SolverInstallPlan.SolverInstallPlan
exResolve
  db
  exts
  langs
  pkgConfigDb
  targets
  mbj
  countConflicts
  fineGrainedConflicts
  minimizeConflictSet
  indepGoals
  prefOldest
  reorder
  allowBootLibInstalls
  onlyConstrained
  enableBj
  solveExes
  goalOrder
  constraints
  prefs
  verbosity
  enableAllTests =
    resolveDependencies C.buildPlatform compiler pkgConfigDb Modular params
    where
      defaultCompiler = C.unknownCompilerInfo C.buildCompilerId C.NoAbiTag
      compiler =
        defaultCompiler
          { C.compilerInfoExtensions = exts
          , C.compilerInfoLanguages = langs
          }
      (inst, avai) = partitionEithers db
      instIdx = exInstIdx inst
      avaiIdx =
        SourcePackageDb
          { packageIndex = exAvIdx avai
          , packagePreferences = Map.empty
          }
      enableTests
        | asBool enableAllTests =
            fmap
              ( \p ->
                  PackageConstraint
                    (scopeToplevel (C.mkPackageName p))
                    (PackagePropertyStanzas [TestStanzas])
              )
              (exDbPkgs db)
        | otherwise = []
      targets' = fmap (\p -> NamedPackage (C.mkPackageName p) []) targets
      params =
        addConstraints (fmap toConstraint constraints) $
          addConstraints (fmap toLpc enableTests) $
            addPreferences (fmap toPref prefs) $
              setCountConflicts countConflicts $
                setFineGrainedConflicts fineGrainedConflicts $
                  setMinimizeConflictSet minimizeConflictSet $
                    setIndependentGoals indepGoals $
                      (if asBool prefOldest then setPreferenceDefault PreferAllOldest else id) $
                        setReorderGoals reorder $
                          setMaxBackjumps mbj $
                            setAllowBootLibInstalls allowBootLibInstalls $
                              setOnlyConstrained onlyConstrained $
                                setEnableBackjumping enableBj $
                                  setSolveExecutables solveExes $
                                    setGoalOrder goalOrder $
                                      setSolverVerbosity verbosity $
                                        standardInstallPolicy instIdx avaiIdx targets'
      toLpc pc = LabeledPackageConstraint pc ConstraintSourceUnknown

      toConstraint (ExVersionConstraint scope v) =
        toLpc $ PackageConstraint scope (PackagePropertyVersion v)
      toConstraint (ExFlagConstraint scope fn b) =
        toLpc $ PackageConstraint scope (PackagePropertyFlags (C.mkFlagAssignment [(C.mkFlagName fn, b)]))
      toConstraint (ExStanzaConstraint scope stanzas) =
        toLpc $ PackageConstraint scope (PackagePropertyStanzas stanzas)

      toPref (ExPkgPref n v) = PackageVersionPreference (C.mkPackageName n) v
      toPref (ExStanzaPref n stanzas) = PackageStanzasPreference (C.mkPackageName n) stanzas

extractInstallPlan
  :: CI.SolverInstallPlan.SolverInstallPlan
  -> [(ExamplePkgName, ExamplePkgVersion)]
extractInstallPlan = catMaybes . map confPkg . CI.SolverInstallPlan.toList
  where
    confPkg :: CI.SolverInstallPlan.SolverPlanPackage -> Maybe (String, Int)
    confPkg (CI.SolverInstallPlan.Configured pkg) = srcPkg pkg
    confPkg _ = Nothing

    srcPkg :: SolverPackage UnresolvedPkgLoc -> Maybe (String, Int)
    srcPkg cpkg =
      let C.PackageIdentifier pn ver = C.packageId (solverPkgSource cpkg)
       in (\vn -> (C.unPackageName pn, vn)) <$> safeHead (C.versionNumbers ver)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Run Progress computation
runProgress :: Progress step e a -> ([step], Either e a)
runProgress = go
  where
    go (Step s p) = let (ss, result) = go p in (s : ss, result)
    go (Fail e) = ([], Left e)
    go (Done a) = ([], Right a)

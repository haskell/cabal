{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A DSL for writing package databases and running the modular solver on
-- them in tests.
--
-- A database is a list of installed packages ('Left') and available packages
-- ('Right'). Package versions are single numbers:
--
-- > db :: ExampleDb
-- > db =
-- >   [ Left $ exInst "base" 1 "base-1" []
-- >   , Right $ exAv "A" 1 [ExAny "base"]
-- >   , Right $ exAv "A" 2 [ExFix "B" 1] `withTest` exTest "test" [ExAny "C"]
-- >   , Right $ exAv "B" 1 []
-- >   , Right $ exAv "C" 1 []
-- >   ]
--
-- "UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils" makes a test case
-- from a database.
module UnitTests.Distribution.Solver.Modular.DSL
  ( -- * Package databases
    ExampleDb
  , ExampleAvailable (..)
  , ExampleInstalled (..)
  , exAv
  , exAvNoLibrary
  , exInst

    -- * Dependencies
  , ExampleDependency (..)
  , exFlagged
  , Dependencies (..)
  , dependencies
  , publicDependencies
  , unbuildableDependencies

    -- * Components
  , ExSubLib (..)
  , ExTest (..)
  , ExExe (..)
  , exSubLib
  , exTest
  , exExe
  , withSubLibrary
  , withSubLibraries
  , withTest
  , withTests
  , withExe
  , withExes
  , withSetupDeps

    -- * Flags
  , ExFlag (..)
  , declareFlags

    -- * Constraints, preferences and goals
  , ExConstraint (..)
  , ExPreference (..)
  , ExampleVar (..)
  , ExampleQualifier (..)
  , EnableAllTests (..)

    -- * Running the solver
  , exResolve
  , extractInstallPlan
  , runProgress

    -- * Names and versions
  , ExamplePkgName
  , ExamplePkgVersion
  , ExamplePkgHash
  , ExampleFlagName
  , ExampleSubLibName
  , ExampleTestName
  , ExampleExeName
  , ExampleVersionRange
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

import qualified Distribution.Compat.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L

-- cabal-install
import Distribution.Client.Dependency
import qualified Distribution.Client.SolverInstallPlan as CI.SolverInstallPlan
import Distribution.Client.Types

import Data.Foldable (fold)
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
import Distribution.Types.UnitId (UnitId)

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

-- | The name of a package.
type ExamplePkgName = String

-- | The version of a package, which 'mkSimpleVersion' makes into a version
-- with one component.
type ExamplePkgVersion = Int

-- | The identifier of an installed package, which is any string that is unique
-- in the database.
type ExamplePkgHash = String

-- | The name of a flag of a package.
type ExampleFlagName = String

-- | The name of a sub-library of a package.
type ExampleSubLibName = String

-- | The name of a test suite of a package.
type ExampleTestName = String

-- | The name of an executable of a package.
type ExampleExeName = String

-- | A version range for a constraint or preference. Make one with
-- 'mkVersionRange'.
type ExampleVersionRange = C.VersionRange

-- | The dependencies of a component, with whether the component is buildable
-- and, for a sub-library, whether it is visible to other packages. A component can
-- have these under a flag, see 'ExFlagged'.
--
-- The dependencies of the setup script of a package are held in one of these
-- too, see 'withSetupDeps', where only the list of dependencies has a meaning.
--
-- Combining two with '<>' joins their dependencies. The result is buildable
-- if both are, and public if either is.
data Dependencies = Dependencies
  { depsVisibility :: C.LibraryVisibility
  -- ^ Private unless set, as with 'publicDependencies'.
  , depsIsBuildable :: Bool
  -- ^ Buildable unless unset, as with 'unbuildableDependencies'.
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

-- | The dependencies of a component that is buildable and, for a sub-library,
-- private. This also makes the dependencies of a setup script.
dependencies :: [ExampleDependency] -> Dependencies
dependencies deps = mempty{depsExampleDependencies = deps}

-- | An empty list of dependencies, for a sub-library that is visible to other
-- packages.
publicDependencies :: Dependencies
publicDependencies = mempty{depsVisibility = C.LibraryVisibilityPublic}

-- | An empty list of dependencies, for a component that is not buildable.
unbuildableDependencies :: Dependencies
unbuildableDependencies = mempty{depsIsBuildable = False}

-- | Something that a package requires: another package, a build tool, a
-- language extension, a language or a pkg-config package. The solver calls all
-- of these dependencies.
--
-- It is usually a dependency of one component of the package. It can also be a
-- dependency of the setup script, see 'withSetupDeps', where it must not be
-- under a flag.
--
-- A dependency on a package or a build tool gives the solver another package to
-- choose a version for. An extension, a language or a pkg-config package is
-- checked against the compiler or the pkg-config database, and rules a package
-- version out when it is not met.
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
  | -- | Dependencies indexed by a flag: the first when the flag is true and
    -- the second when it is false. See 'exFlagged'.
    ExFlagged ExampleFlagName Dependencies Dependencies
  | -- | Dependency on a language extension
    ExExt Extension
  | -- | Dependency on a language version
    ExLang Language
  | -- | Dependency on a fixed version of a pkg-config package
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

-- | A sub-library to add to a package with 'withSubLibrary'.
data ExSubLib = ExSubLib ExampleSubLibName Dependencies

-- | A test suite to add to a package with 'withTest'.
data ExTest = ExTest ExampleTestName Dependencies

-- | An executable to add to a package with 'withExe'.
data ExExe = ExExe ExampleExeName Dependencies

-- | A buildable, private sub-library with the given dependencies.
exSubLib :: ExampleSubLibName -> [ExampleDependency] -> ExSubLib
exSubLib name deps = ExSubLib name (dependencies deps)

-- | A buildable test suite with the given dependencies.
exTest :: ExampleTestName -> [ExampleDependency] -> ExTest
exTest name deps = ExTest name (dependencies deps)

-- | A buildable executable with the given dependencies.
exExe :: ExampleExeName -> [ExampleDependency] -> ExExe
exExe name deps = ExExe name (dependencies deps)

-- | Dependencies under a flag: the first list when the flag is true and the
-- second when it is false.
exFlagged
  :: ExampleFlagName
  -> [ExampleDependency]
  -> [ExampleDependency]
  -> ExampleDependency
exFlagged n t e = ExFlagged n (dependencies t) (dependencies e)

-- | A constraint that the solver must satisfy, on the packages in a scope.
data ExConstraint
  = -- | The version of a package must be in the range.
    ExVersionConstraint ConstraintScope ExampleVersionRange
  | -- | A flag of a package must have the value.
    ExFlagConstraint ConstraintScope ExampleFlagName Bool
  | -- | The stanzas of a package, such as its tests, must be enabled.
    ExStanzaConstraint ConstraintScope [OptionalStanza]
  deriving (Show)

-- | A preference that the solver satisfies when it can.
data ExPreference
  = -- | Prefer versions of a package that are in the range.
    ExPkgPref ExamplePkgName ExampleVersionRange
  | -- | Prefer to enable the stanzas of a package, such as its tests.
    ExStanzaPref ExamplePkgName [OptionalStanza]
  deriving (Show)

-- | A source package in an 'ExampleDb': one that is available to be built, as
-- opposed to an installed package ('ExampleInstalled'). Make one with 'exAv'
-- or 'exAvNoLibrary', then add to it with functions such as 'withTest' and
-- 'declareFlags'.
data ExampleAvailable = ExAv
  { exAvName :: ExamplePkgName
  , exAvVersion :: ExamplePkgVersion
  , exAvDeps :: ComponentDeps Dependencies
  -- ^ The dependencies of each component, which also says which components
  -- the package has.
  , exAvFlags :: [ExFlag]
  -- ^ Setting flags here is only necessary to override the default values of
  -- the fields in C.Flag.
  }
  deriving (Show)

-- | A variable that the solver chooses a value for, used to give the order in
-- which it should choose them.
data ExampleVar
  = -- | The version of a package.
    P ExampleQualifier ExamplePkgName
  | -- | The value of a flag of a package.
    F ExampleQualifier ExamplePkgName ExampleFlagName
  | -- | Whether a stanza of a package, such as its tests, is enabled.
    S ExampleQualifier ExamplePkgName OptionalStanza

-- | Which of the solver's goals for a package an 'ExampleVar' is about. The
-- solver can have several goals for one package, each with its own qualifier,
-- and may choose a different version for each.
data ExampleQualifier
  = -- | The package as a dependency of the targets.
    QualNone
  | -- | The package as a dependency of the named target, when the targets are
    -- solved as independent goals.
    QualIndep ExamplePkgName
  | -- | The package as a dependency of the setup script of the named package.
    QualSetup ExamplePkgName
  | -- | As 'QualSetup', when the targets are solved as independent goals. The
    -- two package names are the build target and the package containing the
    -- setup script.
    QualIndepSetup ExamplePkgName ExamplePkgName
  | -- | The package as a dependency of an executable that is used as a build
    -- tool. The two package names are the package depending on the exe and
    -- the package containing the exe.
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

-- | Adds a sub-library to a package.
withSubLibrary :: ExampleAvailable -> ExSubLib -> ExampleAvailable
withSubLibrary ex lib = withSubLibraries ex [lib]

-- | Adds sub-libraries to a package.
withSubLibraries :: ExampleAvailable -> [ExSubLib] -> ExampleAvailable
withSubLibraries ex libs =
  let subLibCDs =
        CD.fromList
          [ (CD.ComponentSubLib $ C.mkUnqualComponentName name, deps)
          | ExSubLib name deps <- libs
          ]
   in ex{exAvDeps = exAvDeps ex <> subLibCDs}

-- | Adds dependencies for the setup script of a package, which also gives the
-- package the @Custom@ build type.
withSetupDeps :: ExampleAvailable -> [ExampleDependency] -> ExampleAvailable
withSetupDeps ex setupDeps =
  ex
    { exAvDeps = exAvDeps ex <> CD.fromSetupDeps (dependencies setupDeps)
    }

-- | Adds a test suite to a package.
withTest :: ExampleAvailable -> ExTest -> ExampleAvailable
withTest ex test = withTests ex [test]

-- | Adds test suites to a package.
withTests :: ExampleAvailable -> [ExTest] -> ExampleAvailable
withTests ex tests =
  let testCDs =
        CD.fromList
          [ (CD.ComponentTest $ C.mkUnqualComponentName name, deps)
          | ExTest name deps <- tests
          ]
   in ex{exAvDeps = exAvDeps ex <> testCDs}

-- | Adds an executable to a package.
withExe :: ExampleAvailable -> ExExe -> ExampleAvailable
withExe ex exe = withExes ex [exe]

-- | Adds executables to a package.
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

type DependencyTree a = C.CondTree C.ConfVar a

type DependencyComponent a = C.CondBranch C.ConfVar a

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
                names = extractFlags $ fold (exAvDeps ex)
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
                      , C.licenseFiles = [C.makeRelativePathEx "LICENSE"]
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
        let checks = C.checkPackage (srcpkgDescription package)
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
        , C.modulePath = C.makeRelativePathEx "Main.hs"
        }

    defaultTest :: C.TestSuite
    defaultTest =
      mempty
        { C.testBuildInfo = defaultTopLevelBuildInfo
        , C.testInterface =
            C.TestSuiteExeV10 (C.mkVersion [1, 0]) $
              C.makeRelativePathEx "Test.hs"
        }

    defaultBenchmark :: C.Benchmark
    defaultBenchmark =
      mempty
        { C.benchmarkBuildInfo = defaultTopLevelBuildInfo
        , C.benchmarkInterface =
            C.BenchmarkExeV10 (C.mkVersion [1, 0]) $
              C.makeRelativePathEx "Benchmark.hs"
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
        go ExAny{} = []
        go ExFix{} = []
        go ExRange{} = []
        go ExSubLibAny{} = []
        go ExSubLibFix{} = []
        go ExBuildToolAny{} = []
        go ExBuildToolFix{} = []
        go ExLegacyBuildToolAny{} = []
        go ExLegacyBuildToolFix{} = []
        go (ExFlagged f a b) = f : extractFlags a ++ extractFlags b
        go ExExt{} = []
        go ExLang{} = []
        go ExPkg{} = []

    -- Convert 'Dependencies' into a tree of a specific component type, using
    -- the given top level component and function for creating a component at
    -- any level.
    mkTopLevelCondTree
      :: forall a
       . (Semigroup a, L.HasBuildInfo a)
      => a
      -> (C.LibraryVisibility -> C.BuildInfo -> a)
      -> Dependencies
      -> DependencyTree a
    mkTopLevelCondTree defaultTopLevel mkComponent deps =
      let condNode :: DependencyTree a
          condNode = mkCondTree mkComponent deps
       in condNode{C.condTreeData = defaultTopLevel <> C.condTreeData condNode}

    -- Convert 'Dependencies' into a tree of a specific component type, using
    -- the given function to generate each component.
    mkCondTree :: forall a. L.HasBuildInfo a => (C.LibraryVisibility -> C.BuildInfo -> a) -> Dependencies -> DependencyTree a
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
            { C.condTreeData = L.set L.targetBuildDepends (map mkDirect directDeps) component
            , C.condTreeComponents = map (mkFlagged mkComponent) flaggedDeps
            }

    mkDirect :: (ExamplePkgName, C.LibraryName, C.VersionRange) -> C.Dependency
    mkDirect (dep, name, vr) = C.Dependency (C.mkPackageName dep) vr (NonEmptySet.singleton name)

    mkFlagged
      :: forall a
       . L.HasBuildInfo a
      => (C.LibraryVisibility -> C.BuildInfo -> a)
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

-- | The version with the number as its only component.
mkSimpleVersion :: ExamplePkgVersion -> C.Version
mkSimpleVersion n = C.mkVersion [n]

mkSimplePkgconfigVersion :: ExamplePkgVersion -> C.PkgconfigVersion
mkSimplePkgconfigVersion = C.versionToPkgconfigVersion . mkSimpleVersion

-- | The range of versions from the first, inclusive, to the second, exclusive.
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
    , pkgVersion = C.mkVersion [exAvVersion ex]
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
    , pkgVersion = C.mkVersion [exInstVersion ex]
    }

exAvIdx :: [ExampleAvailable] -> CI.PackageIndex.PackageIndex UnresolvedSourcePackage
exAvIdx = CI.PackageIndex.fromList . map exAvSrcPkg

exInstIdx :: [ExampleInstalled] -> C.PackageIndex.InstalledPackageIndex
exInstIdx = C.PackageIndex.fromList . map exInstInfo

-- | Runs the solver on a database. The steps of the result are the lines of
-- the solver's log, and it ends with an error message or an install plan.
--
-- Tests usually make a @SolverTest@ with
-- "UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils" and don't call
-- this.
exResolve
  :: ExampleDb
  -> Maybe [Extension]
  -- ^ List of extensions supported by the compiler, or Nothing if unknown.
  -> Maybe [Language]
  -- ^ List of languages supported by the compiler, or Nothing if unknown.
  -> Maybe [(C.PackageName, UnitId)]
  -- ^ List of units that are wired in to the compiler
  -> Maybe PC.PkgConfigDb
  -- ^ The pkg-config packages that are installed, or Nothing if unknown.
  -> [ExamplePkgName]
  -- ^ The targets to solve for.
  -> Maybe Int
  -- ^ The most backjumps to allow, or Nothing for no limit.
  -> CountConflicts
  -> FineGrainedConflicts
  -> MinimizeConflictSet
  -> IndependentGoals
  -> PreferVersion
  -> ReorderGoals
  -> AllowBootLibInstalls
  -> OnlyConstrained
  -> EnableBackjumping
  -> SolveExecutables
  -> Maybe (Variable P.QPN -> Variable P.QPN -> Ordering)
  -- ^ The order in which to choose goals, or Nothing for the solver's order.
  -> [ExConstraint]
  -> [ExPreference]
  -> C.Verbosity
  -> EnableAllTests
  -> Progress String String CI.SolverInstallPlan.SolverInstallPlan
exResolve
  db
  exts
  langs
  wiredInUnitIds
  pkgConfigDb
  targets
  mbj
  countConflicts
  fineGrainedConflicts
  minimizeConflictSet
  indepGoals
  prefVersion
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
    resolveDependencies C.buildPlatform compiler pkgConfigDb params
    where
      defaultCompiler = C.unknownCompilerInfo C.buildCompilerId C.NoAbiTag
      compiler =
        defaultCompiler
          { C.compilerInfoExtensions = exts
          , C.compilerInfoLanguages = langs
          , C.compilerInfoWiredInUnitIds = wiredInUnitIds
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
        addConstraints (fmap toConstraint constraints)
          $ addConstraints (fmap toLpc enableTests)
          $ addPreferences (fmap toPref prefs)
          $ setCountConflicts countConflicts
          $ setFineGrainedConflicts fineGrainedConflicts
          $ setMinimizeConflictSet minimizeConflictSet
          $ setIndependentGoals indepGoals
          $ ( case prefVersion of
                PreferOldest -> setPreferenceDefault PreferAllOldest
                PreferLatest -> setPreferenceDefault PreferAllLatest
                PreferInstalledOrLatest -> setPreferenceDefault PreferLatestForSelected
            )
          $ setReorderGoals reorder
          $ setMaxBackjumps mbj
          $ setAllowBootLibInstalls allowBootLibInstalls
          $ setOnlyConstrained onlyConstrained
          $ setEnableBackjumping enableBj
          $ setSolveExecutables solveExes
          $ setGoalOrder goalOrder
          $ setSolverVerbosity (C.verbosityLevel verbosity)
          $ standardInstallPolicy instIdx avaiIdx targets'
      toLpc pc = LabeledPackageConstraint pc ConstraintSourceUnknown

      toConstraint (ExVersionConstraint scope v) =
        toLpc $ PackageConstraint scope (PackagePropertyVersion v)
      toConstraint (ExFlagConstraint scope fn b) =
        toLpc $ PackageConstraint scope (PackagePropertyFlags (C.mkFlagAssignment [(C.mkFlagName fn, b)]))
      toConstraint (ExStanzaConstraint scope stanzas) =
        toLpc $ PackageConstraint scope (PackagePropertyStanzas stanzas)

      toPref (ExPkgPref n v) = PackageVersionPreference (C.mkPackageName n) v
      toPref (ExStanzaPref n stanzas) = PackageStanzasPreference (C.mkPackageName n) stanzas

-- | The source packages that an install plan builds, with their versions.
-- Installed packages that the plan uses are left out.
extractInstallPlan
  :: CI.SolverInstallPlan.SolverInstallPlan
  -> [(ExamplePkgName, ExamplePkgVersion)]
extractInstallPlan = mapMaybe confPkg . CI.SolverInstallPlan.toList
  where
    confPkg :: CI.SolverInstallPlan.SolverPlanPackage -> Maybe (String, Int)
    confPkg (CI.SolverInstallPlan.Configured pkg) = srcPkg pkg
    confPkg _ = Nothing

    srcPkg :: SolverPackage UnresolvedPkgLoc -> Maybe (String, Int)
    srcPkg cpkg =
      let C.PackageIdentifier pn ver = C.packageId (solverPkgSource cpkg)
       in (C.unPackageName pn,) <$> safeHead (C.versionNumbers ver)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Runs a computation to its end, collecting its steps and its result.
runProgress :: Progress step e a -> ([step], Either e a)
runProgress = go
  where
    go (Step s p) = let (ss, result) = go p in (s : ss, result)
    go (Fail e) = ([], Left e)
    go (Done a) = ([], Right a)

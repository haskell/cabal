{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module UnitTests.Distribution.Client.Dependency.Modular.Solver (tests, options) where

-- base
import Control.Monad
import Data.Maybe (catMaybes, isNothing)
import Data.Either (partitionEithers)
import Data.Proxy
import Data.Typeable
import Data.Version
import qualified Data.Map as Map

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

-- test-framework
import Test.Tasty as TF
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.Options

-- Cabal
import qualified Distribution.Compiler             as C
import qualified Distribution.InstalledPackageInfo as C
import qualified Distribution.Package              as C hiding (HasInstalledPackageId(..))
import qualified Distribution.PackageDescription   as C
import qualified Distribution.Simple.PackageIndex  as C.PackageIndex
import qualified Distribution.System               as C
import qualified Distribution.Version              as C

-- cabal-install
import Distribution.Client.Dependency
import Distribution.Client.Dependency.Types
import Distribution.Client.Types
import qualified Distribution.Client.InstallPlan  as CI.InstallPlan
import qualified Distribution.Client.PackageIndex as CI.PackageIndex

tests :: [TF.TestTree]
tests = [
      testGroup "Simple dependencies" [
          runTest $         mkTest db1 "alreadyInstalled"   ["A"]      (Just [])
        , runTest $         mkTest db1 "installLatest"      ["B"]      (Just [("B", 2)])
        , runTest $         mkTest db1 "simpleDep1"         ["C"]      (Just [("B", 1), ("C", 1)])
        , runTest $         mkTest db1 "simpleDep2"         ["D"]      (Just [("B", 2), ("D", 1)])
        , runTest $         mkTest db1 "failTwoVersions"    ["C", "D"] Nothing
        , runTest $ indep $ mkTest db1 "indepTwoVersions"   ["C", "D"] (Just [("B", 1), ("B", 2), ("C", 1), ("D", 1)])
        , runTest $ indep $ mkTest db1 "aliasWhenPossible1" ["C", "E"] (Just [("B", 1), ("C", 1), ("E", 1)])
        , runTest $ indep $ mkTest db1 "aliasWhenPossible2" ["D", "E"] (Just [("B", 2), ("D", 1), ("E", 1)])
        , runTest $ indep $ mkTest db2 "aliasWhenPossible3" ["C", "D"] (Just [("A", 1), ("A", 2), ("B", 1), ("B", 2), ("C", 1), ("D", 1)])
        , runTest $         mkTest db1 "buildDepAgainstOld" ["F"]      (Just [("B", 1), ("E", 1), ("F", 1)])
        , runTest $         mkTest db1 "buildDepAgainstNew" ["G"]      (Just [("B", 2), ("E", 1), ("G", 1)])
        , runTest $ indep $ mkTest db1 "multipleInstances"  ["F", "G"] Nothing
        ]
    , testGroup "Flagged dependencies" [
          runTest $         mkTest db3 "forceFlagOn"  ["C"]      (Just [("A", 1), ("B", 1), ("C", 1)])
        , runTest $         mkTest db3 "forceFlagOff" ["D"]      (Just [("A", 2), ("B", 1), ("D", 1)])
        , runTest $ indep $ mkTest db3 "linkFlags1"   ["C", "D"] Nothing
        , runTest $ indep $ mkTest db4 "linkFlags2"   ["C", "D"] Nothing
        ]
    , testGroup "Stanzas" [
          runTest $         mkTest db5 "simpleTest1" ["C"]      (Just [("A", 2), ("C", 1)])
        , runTest $         mkTest db5 "simpleTest2" ["D"]      Nothing
        , runTest $         mkTest db5 "simpleTest3" ["E"]      (Just [("A", 1), ("E", 1)])
        , runTest $         mkTest db5 "simpleTest4" ["F"]      Nothing -- TODO
        , runTest $         mkTest db5 "simpleTest5" ["G"]      (Just [("A", 2), ("G", 1)])
        , runTest $         mkTest db5 "simpleTest6" ["E", "G"] Nothing
        , runTest $ indep $ mkTest db5 "simpleTest7" ["E", "G"] (Just [("A", 1), ("A", 2), ("E", 1), ("G", 1)])
        , runTest $         mkTest db6 "depsWithTests1" ["C"]      (Just [("A", 1), ("B", 1), ("C", 1)])
        , runTest $ indep $ mkTest db6 "depsWithTests2" ["C", "D"] (Just [("A", 1), ("B", 1), ("C", 1), ("D", 1)])
        ]
    ]
  where
    indep test = test { testIndepGoals = True }

{-------------------------------------------------------------------------------
  Solver tests
-------------------------------------------------------------------------------}

data SolverTest = SolverTest {
    testLabel      :: String
  , testTargets    :: [String]
  , testResult     :: Maybe [(String, Int)]
  , testIndepGoals :: Bool
  , testDb         :: ExampleDb
  }

mkTest :: ExampleDb
       -> String
       -> [String]
       -> Maybe [(String, Int)]
       -> SolverTest
mkTest db label targets result = SolverTest {
    testLabel      = label
  , testTargets    = targets
  , testResult     = result
  , testIndepGoals = False
  , testDb         = db
  }

runTest :: SolverTest -> TF.TestTree
runTest SolverTest{..} = askOption $ \(OptionShowSolverLog showSolverLog) ->
    testCase testLabel $ do
      let (_msgs, result) = exResolve testDb testTargets testIndepGoals
      when showSolverLog $ mapM_ putStrLn _msgs
      case result of
        Left  err  -> assertBool ("Unexpected error:\n" ++ err) (isNothing testResult)
        Right plan -> assertEqual "" testResult (Just (extractInstallPlan plan))

{-------------------------------------------------------------------------------
  Specific example database for the tests
-------------------------------------------------------------------------------}

db1 :: ExampleDb
db1 =
    let a = ExInst "A" 1 "A-1" []
    in [ Left a
       , Right $ ExAv "B" 1 [ExAny "A"]
       , Right $ ExAv "B" 2 [ExAny "A"]
       , Right $ ExAv "C" 1 [ExFix "B" 1]
       , Right $ ExAv "D" 1 [ExFix "B" 2]
       , Right $ ExAv "E" 1 [ExAny "B"]
       , Right $ ExAv "F" 1 [ExFix "B" 1, ExAny "E"]
       , Right $ ExAv "G" 1 [ExFix "B" 2, ExAny "E"]
       , Right $ ExAv "Z" 1 []
       ]

-- In this example, we _can_ install C and D as independent goals, but we have
-- to pick two diferent versions for B (arbitrarily)
db2 :: ExampleDb
db2 = [
    Right $ ExAv "A" 1 []
  , Right $ ExAv "A" 2 []
  , Right $ ExAv "B" 1 [ExAny "A"]
  , Right $ ExAv "B" 2 [ExAny "A"]
  , Right $ ExAv "C" 1 [ExAny "B", ExFix "A" 1]
  , Right $ ExAv "D" 1 [ExAny "B", ExFix "A" 2]
  ]

db3 :: ExampleDb
db3 = [
     Right $ ExAv "A" 1 []
   , Right $ ExAv "A" 2 []
   , Right $ ExAv "B" 1 [ExFlag "flagB" [ExFix "A" 1] [ExFix "A" 2]]
   , Right $ ExAv "C" 1 [ExFix "A" 1, ExAny "B"]
   , Right $ ExAv "D" 1 [ExFix "A" 2, ExAny "B"]
   ]

-- | Like exampleDb2, but the flag picks a different package rather than a
-- different package version
--
-- In exampleDb2 we cannot install C and D as independent goals because:
--
-- * The multiple instance restriction says C and D _must_ share B
-- * Since C relies on A.1, C needs B to be compiled with flagB on
-- * Since D relies on A.2, D needs B to be compiled with flagsB off
-- * Hence C and D have incompatible requirements on B's flags.
--
-- However, _even_ if we don't check explicitly that we pick the same flag
-- assignment for 0.B and 1.B, we will still detect the problem because
-- 0.B depends on 0.A-1, 1.B depends on 1.A-2, hence we cannot link 0.A to
-- 1.B and therefore we cannot link 0.B to 1.B.
--
-- In exampleDb3 the situation however is trickier. We again cannot install
-- packages C and D as independent goals because:
--
-- * As above, the multiple instance restriction says that C and D _must_ share B
-- * Since C relies on Ax-2, it requires B to be compiled with flagB off
-- * Since D relies on Ay-2, it requires B to be compiled with flagB on
-- * Hence C and D have incompatible requirements on B's flags.
--
-- But now this requirement is more indirect. If we only check dependencies
-- we don't see the problem:
--
-- * We link 0.B to 1.B
-- * 0.B relies on Ay.1
-- * 1.B relies on Ax.1
--
-- We will insist that 0.Ay will be linked to 1.Ay, and 0.Ax to 1.A, but since
-- we only ever assign to one of these, these constraints are never broken.
db4 :: ExampleDb
db4 = [
     Right $ ExAv "Ax" 1 []
   , Right $ ExAv "Ax" 2 []
   , Right $ ExAv "Ay" 1 []
   , Right $ ExAv "Ay" 2 []
   , Right $ ExAv "B"  1 [ExFlag "flagB" [ExFix "Ax" 1] [ExFix "Ay" 1]]
   , Right $ ExAv "C"  1 [ExFix "Ax" 2, ExAny "B"]
   , Right $ ExAv "D"  1 [ExFix "Ay" 2, ExAny "B"]
   ]

-- | Some tests involving testsuites
--
-- Note that in this test framework test suites are always enabled; if you
-- want to test without test suites just set up a test database without
-- test suites.
--
-- * C depends on A (through its test suite)
-- * D depends on B-2 (through its test suite), but B-2 is unavailable
-- * E depends on A-1 directly and on A through its test suite. We prefer
--     to use A-1 for the test suite in this case.
-- * F depends on A-1 directly and on A-2 through its test suite. In this
--     case we currently fail to install F, although strictly speaking
--     test suites should be considered independent goals.
-- * G is like E, but for version A-2. This means that if we cannot install
--     E and G together, unless we regard them as independent goals.
db5 :: ExampleDb
db5 = [
    Right $ ExAv "A" 1 []
  , Right $ ExAv "A" 2 []
  , Right $ ExAv "B" 1 []
  , Right $ ExAv "C" 1 [ExTest "testC" [ExAny "A"]]
  , Right $ ExAv "D" 1 [ExTest "testD" [ExFix "B" 2]]
  , Right $ ExAv "E" 1 [ExFix "A" 1, ExTest "testE" [ExAny "A"]]
  , Right $ ExAv "F" 1 [ExFix "A" 1, ExTest "testF" [ExFix "A" 2]]
  , Right $ ExAv "G" 1 [ExFix "A" 2, ExTest "testG" [ExAny "A"]]
  ]

-- Now the _dependencies_ have test suites
--
-- * Installing C is a simple example. C wants version 1 of A, but depends on
--   B, and B's testsuite depends on an any version of A. In this case we prefer
--   to link (if we don't regard test suites as independent goals then of course
--   linking here doesn't even come into it).
-- * Installing [C, D] means that we prefer to link B -- depending on how we
--   set things up, this means that we should also link their test suites.
db6 :: ExampleDb
db6 = [
    Right $ ExAv "A" 1 []
  , Right $ ExAv "A" 2 []
  , Right $ ExAv "B" 1 [ExTest "testA" [ExAny "A"]]
  , Right $ ExAv "C" 1 [ExFix "A" 1, ExAny "B"]
  , Right $ ExAv "D" 1 [ExAny "B"]
  ]

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
    `GenericPackageDescription` can be returned into a `PackageDescription` in
    two ways:

      a. `finalizePackageDescription` does the proper translation, by taking
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

type ExamplePkgName    = String
type ExamplePkgVersion = Int
type ExamplePkgHash    = String  -- for example "installed" packages
type ExampleFlagName   = String
type ExampleTestName   = String

data ExampleDependency =
    -- | Simple dependency on any version
    ExAny ExamplePkgName

    -- | Simple dependency on a fixed version
  | ExFix ExamplePkgName ExamplePkgVersion

    -- | Dependencies indexed by a flag
  | ExFlag ExampleFlagName [ExampleDependency] [ExampleDependency]

    -- | Dependency if tests are enabled
  | ExTest ExampleTestName [ExampleDependency]

data ExampleAvailable = ExAv {
    exAvName    :: ExamplePkgName
  , exAvVersion :: ExamplePkgVersion
  , exAvDeps    :: [ExampleDependency]
  }

data ExampleInstalled = ExInst {
    exInstName         :: ExamplePkgName
  , exInstVersion      :: ExamplePkgVersion
  , exInstHash         :: ExamplePkgHash
  , exInstBuildAgainst :: [ExampleInstalled]
  }

type ExampleDb = [Either ExampleInstalled ExampleAvailable]

type DependencyTree a = C.CondTree C.ConfVar [C.Dependency] a

exDbPkgs :: ExampleDb -> [ExamplePkgName]
exDbPkgs = map (either exInstName exAvName)

exAvSrcPkg :: ExampleAvailable -> SourcePackage
exAvSrcPkg ex =
    let (libraryDeps, testSuites) = splitTopLevel (exAvDeps ex)
    in SourcePackage {
           packageInfoId        = exAvPkgId ex
         , packageSource        = LocalTarballPackage "<<path>>"
         , packageDescrOverride = Nothing
         , packageDescription   = C.GenericPackageDescription{
               C.packageDescription = C.emptyPackageDescription {
                   C.package      = exAvPkgId ex
                 , C.library      = error "not yet configured: library"
                 , C.executables  = error "not yet configured: executables"
                 , C.testSuites   = error "not yet configured: testSuites"
                 , C.benchmarks   = error "not yet configured: benchmarks"
                 , C.buildDepends = error "not yet configured: buildDepends"
                 }
             , C.genPackageFlags = concatMap extractFlags (exAvDeps ex)
             , C.condLibrary     = Just $ mkCondTree libraryDeps
             , C.condExecutables = []
             , C.condTestSuites  = map (\(t, deps) -> (t, mkCondTree deps)) testSuites
             , C.condBenchmarks  = []
             }
         }
  where
    splitTopLevel :: [ExampleDependency]
                  -> ( [ExampleDependency]
                     , [(ExampleTestName, [ExampleDependency])]
                     )
    splitTopLevel []                = ([], [])
    splitTopLevel (ExTest t a:deps) = let (other, testSuites) = splitTopLevel deps
                                      in (other, (t, a):testSuites)
    splitTopLevel (dep:deps)        = let (other, testSuites) = splitTopLevel deps
                                      in (dep:other, testSuites)

    extractFlags :: ExampleDependency -> [C.Flag]
    extractFlags (ExAny _)      = []
    extractFlags (ExFix _ _)    = []
    extractFlags (ExFlag f a b) = C.MkFlag {
                                      C.flagName        = C.FlagName f
                                    , C.flagDescription = ""
                                    , C.flagDefault     = False
                                    , C.flagManual      = False
                                    }
                                : concatMap extractFlags (a ++ b)
    extractFlags (ExTest _ a)   = concatMap extractFlags a

    mkCondTree :: Monoid a => [ExampleDependency] -> DependencyTree a
    mkCondTree deps =
      let (directDeps, flaggedDeps) = splitDeps deps
      in C.CondNode {
             C.condTreeData        = mempty -- irrelevant to the solver
           , C.condTreeConstraints = map mkDirect  directDeps
           , C.condTreeComponents  = map mkFlagged flaggedDeps
           }

    mkDirect :: (ExamplePkgName, Maybe ExamplePkgVersion) -> C.Dependency
    mkDirect (dep, Nothing) = C.Dependency (C.PackageName dep) C.anyVersion
    mkDirect (dep, Just n)  = C.Dependency (C.PackageName dep) (C.thisVersion v)
      where
        v = Version [n, 0, 0] []

    mkFlagged :: Monoid a
              => (ExampleFlagName, [ExampleDependency], [ExampleDependency])
              -> (C.Condition C.ConfVar, DependencyTree a, Maybe (DependencyTree a))
    mkFlagged (f, a, b) = ( C.Var (C.Flag (C.FlagName f))
                          , mkCondTree a
                          , Just (mkCondTree b)
                          )

    splitDeps :: [ExampleDependency]
              -> ( [(ExamplePkgName, Maybe Int)]
                 , [(ExampleFlagName, [ExampleDependency], [ExampleDependency])]
                 )
    splitDeps [] =
      ([], [])
    splitDeps (ExAny p:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, Nothing):directDeps, flaggedDeps)
    splitDeps (ExFix p v:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in ((p, Just v):directDeps, flaggedDeps)
    splitDeps (ExFlag f a b:deps) =
      let (directDeps, flaggedDeps) = splitDeps deps
      in (directDeps, (f, a, b):flaggedDeps)
    splitDeps (ExTest _ _:_) =
      error "Unexpected nested test"

exAvPkgId :: ExampleAvailable -> C.PackageIdentifier
exAvPkgId ex = C.PackageIdentifier {
      pkgName    = C.PackageName (exAvName ex)
    , pkgVersion = Version [exAvVersion ex, 0, 0] []
    }

exInstInfo :: ExampleInstalled -> C.InstalledPackageInfo
exInstInfo ex = C.emptyInstalledPackageInfo {
      C.installedPackageId = C.InstalledPackageId (exInstHash ex)
    , C.sourcePackageId    = exInstPkgId ex
    , C.packageKey         = exInstKey ex
    , C.depends            = map (C.InstalledPackageId . exInstHash)
                                 (exInstBuildAgainst ex)
    }

exInstPkgId :: ExampleInstalled -> C.PackageIdentifier
exInstPkgId ex = C.PackageIdentifier {
      pkgName    = C.PackageName (exInstName ex)
    , pkgVersion = Version [exInstVersion ex, 0, 0] []
    }

exInstKey :: ExampleInstalled -> C.PackageKey
exInstKey ex =
    C.mkPackageKey True
                   (exInstPkgId ex)
                   (map exInstKey (exInstBuildAgainst ex))
                   []

exAvIdx :: [ExampleAvailable] -> CI.PackageIndex.PackageIndex SourcePackage
exAvIdx = CI.PackageIndex.fromList . map exAvSrcPkg

exInstIdx :: [ExampleInstalled] -> C.PackageIndex.InstalledPackageIndex
exInstIdx = C.PackageIndex.fromList . map exInstInfo

exResolve :: ExampleDb
          -> [ExamplePkgName]
          -> Bool
          -> ([String], Either String CI.InstallPlan.InstallPlan)
exResolve db targets indepGoals = runProgress $
    resolveDependencies C.buildPlatform
                        (C.unknownCompilerInfo C.buildCompilerId C.NoAbiTag)
                        Modular
                        params
  where
    (inst, avai) = partitionEithers db
    instIdx      = exInstIdx inst
    avaiIdx      = SourcePackageDb {
                       packageIndex       = exAvIdx avai
                     , packagePreferences = Map.empty
                     }
    enableTests  = map (\p -> PackageConstraintStanzas (C.PackageName p) [TestStanzas])
                       (exDbPkgs db)
    targets'     = map (\p -> NamedPackage (C.PackageName p) []) targets
    params       = addConstraints enableTests
                 $ (standardInstallPolicy instIdx avaiIdx targets') {
                       depResolverIndependentGoals = indepGoals
                     }

extractInstallPlan :: CI.InstallPlan.InstallPlan
                   -> [(ExamplePkgName, ExamplePkgVersion)]
extractInstallPlan = catMaybes . map confPkg . CI.InstallPlan.toList
  where
    confPkg :: CI.InstallPlan.PlanPackage -> Maybe (String, Int)
    confPkg (CI.InstallPlan.Configured pkg) = Just $ srcPkg pkg
    confPkg _                               = Nothing

    srcPkg :: ConfiguredPackage -> (String, Int)
    srcPkg (ConfiguredPackage pkg _flags _stanzas _deps) =
      let C.PackageIdentifier (C.PackageName p) (Version (n:_) _) = packageInfoId pkg
      in (p, n)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Run Progress computation
--
-- Like `runLog`, but for the more general `Progress` type.
runProgress :: Progress step e a -> ([step], Either e a)
runProgress = go
  where
    go (Step s p) = let (ss, result) = go p in (s:ss, result)
    go (Fail e)   = ([], Left e)
    go (Done a)   = ([], Right a)

{-------------------------------------------------------------------------------
  Test options
-------------------------------------------------------------------------------}

options :: [OptionDescription]
options = [
    Option (Proxy :: Proxy OptionShowSolverLog)
  ]

newtype OptionShowSolverLog = OptionShowSolverLog Bool
  deriving Typeable

instance IsOption OptionShowSolverLog where
  defaultValue   = OptionShowSolverLog False
  parseValue     = fmap OptionShowSolverLog . safeRead
  optionName     = return "show-solver-log"
  optionHelp     = return "Show full log from the solver"
  optionCLParser = flagCLParser Nothing (OptionShowSolverLog True)

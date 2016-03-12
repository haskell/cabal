{-# LANGUAGE RecordWildCards #-}
module UnitTests.Distribution.Client.Dependency.Modular.Solver (tests)
       where

-- base
import Control.Monad
import Data.Maybe (isNothing)

import qualified Data.Version         as V
import qualified Distribution.Version as V

-- test-framework
import Test.Tasty as TF
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

-- Cabal
import Language.Haskell.Extension ( Extension(..)
                                  , KnownExtension(..), Language(..))

-- cabal-install
import Distribution.Client.PkgConfigDb (PkgConfigDb, pkgConfigDbFromList)
import Distribution.Client.Dependency.Types (Solver(Modular))
import UnitTests.Distribution.Client.Dependency.Modular.DSL
import UnitTests.Options

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
    , testGroup "Setup dependencies" [
          runTest $ mkTest db7  "setupDeps1" ["B"] (Just [("A", 2), ("B", 1)])
        , runTest $ mkTest db7  "setupDeps2" ["C"] (Just [("A", 2), ("C", 1)])
        , runTest $ mkTest db7  "setupDeps3" ["D"] (Just [("A", 1), ("D", 1)])
        , runTest $ mkTest db7  "setupDeps4" ["E"] (Just [("A", 1), ("A", 2), ("E", 1)])
        , runTest $ mkTest db7  "setupDeps5" ["F"] (Just [("A", 1), ("A", 2), ("F", 1)])
        , runTest $ mkTest db8  "setupDeps6" ["C", "D"] (Just [("A", 1), ("B", 1), ("B", 2), ("C", 1), ("D", 1)])
        , runTest $ mkTest db9  "setupDeps7" ["F", "G"] (Just [("A", 1), ("B", 1), ("B",2 ), ("C", 1), ("D", 1), ("E", 1), ("E", 2), ("F", 1), ("G", 1)])
        , runTest $ mkTest db10 "setupDeps8" ["C"] (Just [("C", 1)])
        ]
    , testGroup "Base shim" [
          runTest $ mkTest db11 "baseShim1" ["A"] (Just [("A", 1)])
        , runTest $ mkTest db12 "baseShim2" ["A"] (Just [("A", 1)])
        , runTest $ mkTest db12 "baseShim3" ["B"] (Just [("B", 1)])
        , runTest $ mkTest db12 "baseShim4" ["C"] (Just [("A", 1), ("B", 1), ("C", 1)])
        , runTest $ mkTest db12 "baseShim5" ["D"] Nothing
        , runTest $ mkTest db12 "baseShim6" ["E"] (Just [("E", 1), ("syb", 2)])
        ]
    , testGroup "Cycles" [
          runTest $ mkTest db14 "simpleCycle1"          ["A"]      Nothing
        , runTest $ mkTest db14 "simpleCycle2"          ["A", "B"] Nothing
        , runTest $ mkTest db14 "cycleWithFlagChoice1"  ["C"]      (Just [("C", 1), ("E", 1)])
        , runTest $ mkTest db15 "cycleThroughSetupDep1" ["A"]      Nothing
        , runTest $ mkTest db15 "cycleThroughSetupDep2" ["B"]      Nothing
        , runTest $ mkTest db15 "cycleThroughSetupDep3" ["C"]      (Just [("C", 2), ("D", 1)])
        , runTest $ mkTest db15 "cycleThroughSetupDep4" ["D"]      (Just [("D", 1)])
        , runTest $ mkTest db15 "cycleThroughSetupDep5" ["E"]      (Just [("C", 2), ("D", 1), ("E", 1)])
        ]
    , testGroup "Extensions" [
          runTest $ mkTestExts [EnableExtension CPP] dbExts1 "unsupported" ["A"] Nothing
        , runTest $ mkTestExts [EnableExtension CPP] dbExts1 "unsupportedIndirect" ["B"] Nothing
        , runTest $ mkTestExts [EnableExtension RankNTypes] dbExts1 "supported" ["A"] (Just [("A",1)])
        , runTest $ mkTestExts (map EnableExtension [CPP,RankNTypes]) dbExts1 "supportedIndirect" ["C"] (Just [("A",1),("B",1), ("C",1)])
        , runTest $ mkTestExts [EnableExtension CPP] dbExts1 "disabledExtension" ["D"] Nothing
        , runTest $ mkTestExts (map EnableExtension [CPP,RankNTypes]) dbExts1 "disabledExtension" ["D"] Nothing
        , runTest $ mkTestExts (UnknownExtension "custom" : map EnableExtension [CPP,RankNTypes]) dbExts1 "supportedUnknown" ["E"] (Just [("A",1),("B",1),("C",1),("E",1)])
        ]
    , testGroup "Languages" [
          runTest $ mkTestLangs [Haskell98] dbLangs1 "unsupported" ["A"] Nothing
        , runTest $ mkTestLangs [Haskell98,Haskell2010] dbLangs1 "supported" ["A"] (Just [("A",1)])
        , runTest $ mkTestLangs [Haskell98] dbLangs1 "unsupportedIndirect" ["B"] Nothing
        , runTest $ mkTestLangs [Haskell98, Haskell2010, UnknownLanguage "Haskell3000"] dbLangs1 "supportedUnknown" ["C"] (Just [("A",1),("B",1),("C",1)])
        ]

     , testGroup "Soft Constraints" [
          runTest $ soft [ ExPref "A" $ mkvrThis 1]      $ mkTest db13 "selectPreferredVersionSimple" ["A"] (Just [("A", 1)])
        , runTest $ soft [ ExPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionSimple2" ["A"] (Just [("A", 2)])
        , runTest $ soft [ ExPref "A" $ mkvrOrEarlier 2
                         , ExPref "A" $ mkvrOrEarlier 1] $ mkTest db13 "selectPreferredVersionMultiple" ["A"] (Just [("A", 1)])
        , runTest $ soft [ ExPref "A" $ mkvrOrEarlier 1
                         , ExPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionMultiple2" ["A"] (Just [("A", 1)])
        , runTest $ soft [ ExPref "A" $ mkvrThis 1
                         , ExPref "A" $ mkvrThis 2] $ mkTest db13 "selectPreferredVersionMultiple3" ["A"] (Just [("A", 2)])
        , runTest $ soft [ ExPref "A" $ mkvrThis 1
                         , ExPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionMultiple4" ["A"] (Just [("A", 1)])
        ]
     , testGroup "Buildable Field" [
          testBuildable "avoid building component with unknown dependency" (ExAny "unknown")
        , testBuildable "avoid building component with unknown extension" (ExExt (UnknownExtension "unknown"))
        , testBuildable "avoid building component with unknown language" (ExLang (UnknownLanguage "unknown"))
        , runTest $ mkTest dbBuildable1 "choose flags that set buildable to false" ["pkg"] (Just [("flag1-false", 1), ("flag2-true", 1), ("pkg", 1)])
        , runTest $ mkTest dbBuildable2 "choose version that sets buildable to false" ["A"] (Just [("A", 1), ("B", 2)])
         ]
    , testGroup "Pkg-config dependencies" [
          runTest $ mkTestPCDepends [] dbPC1 "noPkgs" ["A"] Nothing
        , runTest $ mkTestPCDepends [("pkgA", "0")] dbPC1 "tooOld" ["A"] Nothing
        , runTest $ mkTestPCDepends [("pkgA", "1.0.0"), ("pkgB", "1.0.0")] dbPC1 "pruneNotFound" ["C"] (Just [("A", 1), ("B", 1), ("C", 1)])
        , runTest $ mkTestPCDepends [("pkgA", "1.0.0"), ("pkgB", "2.0.0")] dbPC1 "chooseNewest" ["C"] (Just [("A", 1), ("B", 2), ("C", 1)])
        ]
    , testGroup "Independent goals" [
          runTest $ indep $ mkTest db16 "indepGoals" ["A", "B"] (Just [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2), ("E", 1)])
        ]
    ]
  where
    -- | Combinator to turn on --independent-goals behavior, i.e. solve
    -- for the goals as if we were solving for each goal independently.
    -- (This doesn't really work well at the moment, see #2842)
    indep test      = test { testIndepGoals = IndepGoals True }
    soft prefs test = test { testSoftConstraints = prefs }
    mkvrThis        = V.thisVersion . makeV
    mkvrOrEarlier   = V.orEarlierVersion . makeV
    makeV v         = V.Version [v,0,0] []

{-------------------------------------------------------------------------------
  Solver tests
-------------------------------------------------------------------------------}

data SolverTest = SolverTest {
    testLabel          :: String
  , testTargets        :: [String]
  , testResult         :: Maybe [(String, Int)]
  , testIndepGoals     :: IndepGoals
  , testSoftConstraints :: [ExPreference]
  , testDb             :: ExampleDb
  , testSupportedExts  :: Maybe [Extension]
  , testSupportedLangs :: Maybe [Language]
  , testPkgConfigDb    :: PkgConfigDb
  }

-- | Makes a solver test case, consisting of the following components:
--
--      1. An 'ExampleDb', representing the package database (both
--         installed and remote) we are doing dependency solving over,
--      2. A 'String' name for the test,
--      3. A list '[String]' of package names to solve for
--      4. The expected result, either 'Nothing' if there is no
--         satisfying solution, or a list '[(String, Int)]' of
--         packages to install, at which versions.
--
-- See 'UnitTests.Distribution.Client.Dependency.Modular.DSL' for how
-- to construct an 'ExampleDb', as well as definitions of 'db1' etc.
-- in this file.
mkTest :: ExampleDb
       -> String
       -> [String]
       -> Maybe [(String, Int)]
       -> SolverTest
mkTest = mkTestExtLangPC Nothing Nothing []

mkTestExts :: [Extension]
           -> ExampleDb
           -> String
           -> [String]
           -> Maybe [(String, Int)]
           -> SolverTest
mkTestExts exts = mkTestExtLangPC (Just exts) Nothing []

mkTestLangs :: [Language]
            -> ExampleDb
            -> String
            -> [String]
            -> Maybe [(String, Int)]
            -> SolverTest
mkTestLangs langs = mkTestExtLangPC Nothing (Just langs) []

mkTestPCDepends :: [(String, String)]
                -> ExampleDb
                -> String
                -> [String]
                -> Maybe [(String, Int)]
                -> SolverTest
mkTestPCDepends pkgConfigDb = mkTestExtLangPC Nothing Nothing pkgConfigDb

mkTestExtLangPC :: Maybe [Extension]
                -> Maybe [Language]
                -> [(String, String)]
                -> ExampleDb
                -> String
                -> [String]
                -> Maybe [(String, Int)]
                -> SolverTest
mkTestExtLangPC exts langs pkgConfigDb db label targets result = SolverTest {
    testLabel          = label
  , testTargets        = targets
  , testResult         = result
  , testIndepGoals     = IndepGoals False
  , testSoftConstraints = []
  , testDb             = db
  , testSupportedExts  = exts
  , testSupportedLangs = langs
  , testPkgConfigDb    = pkgConfigDbFromList pkgConfigDb
  }

runTest :: SolverTest -> TF.TestTree
runTest SolverTest{..} = askOption $ \(OptionShowSolverLog showSolverLog) ->
    testCase testLabel $ do
      let (_msgs, result) = exResolve testDb testSupportedExts
                            testSupportedLangs testPkgConfigDb testTargets
                            Modular Nothing testIndepGoals (ReorderGoals False)
                            testSoftConstraints
      when showSolverLog $ mapM_ putStrLn _msgs
      case result of
        Left  err  -> assertBool ("Unexpected error:\n" ++ err) (isNothing testResult)
        Right plan -> assertEqual "" testResult (Just (extractInstallPlan plan))

{-------------------------------------------------------------------------------
  Specific example database for the tests
-------------------------------------------------------------------------------}

db1 :: ExampleDb
db1 =
    let a = exInst "A" 1 "A-1" []
    in [ Left a
       , Right $ exAv "B" 1 [ExAny "A"]
       , Right $ exAv "B" 2 [ExAny "A"]
       , Right $ exAv "C" 1 [ExFix "B" 1]
       , Right $ exAv "D" 1 [ExFix "B" 2]
       , Right $ exAv "E" 1 [ExAny "B"]
       , Right $ exAv "F" 1 [ExFix "B" 1, ExAny "E"]
       , Right $ exAv "G" 1 [ExFix "B" 2, ExAny "E"]
       , Right $ exAv "Z" 1 []
       ]

-- In this example, we _can_ install C and D as independent goals, but we have
-- to pick two diferent versions for B (arbitrarily)
db2 :: ExampleDb
db2 = [
    Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [ExAny "A"]
  , Right $ exAv "B" 2 [ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "B", ExFix "A" 1]
  , Right $ exAv "D" 1 [ExAny "B", ExFix "A" 2]
  ]

db3 :: ExampleDb
db3 = [
     Right $ exAv "A" 1 []
   , Right $ exAv "A" 2 []
   , Right $ exAv "B" 1 [exFlag "flagB" [ExFix "A" 1] [ExFix "A" 2]]
   , Right $ exAv "C" 1 [ExFix "A" 1, ExAny "B"]
   , Right $ exAv "D" 1 [ExFix "A" 2, ExAny "B"]
   ]

-- | Like db3, but the flag picks a different package rather than a
-- different package version
--
-- In db3 we cannot install C and D as independent goals because:
--
-- * The multiple instance restriction says C and D _must_ share B
-- * Since C relies on A-1, C needs B to be compiled with flagB on
-- * Since D relies on A-2, D needs B to be compiled with flagB off
-- * Hence C and D have incompatible requirements on B's flags.
--
-- However, _even_ if we don't check explicitly that we pick the same flag
-- assignment for 0.B and 1.B, we will still detect the problem because
-- 0.B depends on 0.A-1, 1.B depends on 1.A-2, hence we cannot link 0.A to
-- 1.A and therefore we cannot link 0.B to 1.B.
--
-- In db4 the situation however is trickier. We again cannot install
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
-- * 0.B relies on Ay-1
-- * 1.B relies on Ax-1
--
-- We will insist that 0.Ay will be linked to 1.Ay, and 0.Ax to 1.Ax, but since
-- we only ever assign to one of these, these constraints are never broken.
db4 :: ExampleDb
db4 = [
     Right $ exAv "Ax" 1 []
   , Right $ exAv "Ax" 2 []
   , Right $ exAv "Ay" 1 []
   , Right $ exAv "Ay" 2 []
   , Right $ exAv "B"  1 [exFlag "flagB" [ExFix "Ax" 1] [ExFix "Ay" 1]]
   , Right $ exAv "C"  1 [ExFix "Ax" 2, ExAny "B"]
   , Right $ exAv "D"  1 [ExFix "Ay" 2, ExAny "B"]
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
    Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 []
  , Right $ exAv "C" 1 [] `withTest` ExTest "testC" [ExAny "A"]
  , Right $ exAv "D" 1 [] `withTest` ExTest "testD" [ExFix "B" 2]
  , Right $ exAv "E" 1 [ExFix "A" 1] `withTest` ExTest "testE" [ExAny "A"]
  , Right $ exAv "F" 1 [ExFix "A" 1] `withTest` ExTest "testF" [ExFix "A" 2]
  , Right $ exAv "G" 1 [ExFix "A" 2] `withTest` ExTest "testG" [ExAny "A"]
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
    Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 [] `withTest` ExTest "testA" [ExAny "A"]
  , Right $ exAv "C" 1 [ExFix "A" 1, ExAny "B"]
  , Right $ exAv "D" 1 [ExAny "B"]
  ]

-- Packages with setup dependencies
--
-- Install..
-- * B: Simple example, just make sure setup deps are taken into account at all
-- * C: Both the package and the setup script depend on any version of A.
--      In this case we prefer to link
-- * D: Variation on C.1 where the package requires a specific (not latest)
--      version but the setup dependency is not fixed. Again, we prefer to
--      link (picking the older version)
-- * E: Variation on C.2 with the setup dependency the more inflexible.
--      Currently, in this case we do not see the opportunity to link because
--      we consider setup dependencies after normal dependencies; we will
--      pick A.2 for E, then realize we cannot link E.setup.A to A.2, and pick
--      A.1 instead. This isn't so easy to fix (if we want to fix it at all);
--      in particular, considering setup dependencies _before_ other deps is
--      not an improvement, because in general we would prefer to link setup
--      setups to package deps, rather than the other way around. (For example,
--      if we change this ordering then the test for D would start to install
--      two versions of A).
-- * F: The package and the setup script depend on different versions of A.
--      This will only work if setup dependencies are considered independent.
db7 :: ExampleDb
db7 = [
    Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "B" 1 []            `withSetupDeps` [ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "A"  ] `withSetupDeps` [ExAny "A"  ]
  , Right $ exAv "D" 1 [ExFix "A" 1] `withSetupDeps` [ExAny "A"  ]
  , Right $ exAv "E" 1 [ExAny "A"  ] `withSetupDeps` [ExFix "A" 1]
  , Right $ exAv "F" 1 [ExFix "A" 2] `withSetupDeps` [ExFix "A" 1]
  ]

-- If we install C and D together (not as independent goals), we need to build
-- both B.1 and B.2, both of which depend on A.
db8 :: ExampleDb
db8 = [
    Right $ exAv "A" 1 []
  , Right $ exAv "B" 1 [ExAny "A"]
  , Right $ exAv "B" 2 [ExAny "A"]
  , Right $ exAv "C" 1 [] `withSetupDeps` [ExFix "B" 1]
  , Right $ exAv "D" 1 [] `withSetupDeps` [ExFix "B" 2]
  ]

-- Extended version of `db8` so that we have nested setup dependencies
db9 :: ExampleDb
db9 = db8 ++ [
    Right $ exAv "E" 1 [ExAny "C"]
  , Right $ exAv "E" 2 [ExAny "D"]
  , Right $ exAv "F" 1 [] `withSetupDeps` [ExFix "E" 1]
  , Right $ exAv "G" 1 [] `withSetupDeps` [ExFix "E" 2]
  ]

-- Multiple already-installed packages with inter-dependencies, and one package
-- (C) that depends on package A-1 for its setup script and package A-2 as a
-- library dependency.
db10 :: ExampleDb
db10 =
  let rts         = exInst "rts"         1 "rts-inst"         []
      ghc_prim    = exInst "ghc-prim"    1 "ghc-prim-inst"    [rts]
      base        = exInst "base"        1 "base-inst"        [rts, ghc_prim]
      a1          = exInst "A"           1 "A1-inst"          [base]
      a2          = exInst "A"           2 "A2-inst"          [base]
  in [
      Left rts
    , Left ghc_prim
    , Left base
    , Left a1
    , Left a2
    , Right $ exAv "C" 1 [ExFix "A" 2] `withSetupDeps` [ExFix "A" 1]
    ]

-- | Tests for dealing with base shims
db11 :: ExampleDb
db11 =
  let base3 = exInst "base" 3 "base-3-inst" [base4]
      base4 = exInst "base" 4 "base-4-inst" []
  in [
      Left base3
    , Left base4
    , Right $ exAv "A" 1 [ExFix "base" 3]
    ]

-- | Slightly more realistic version of db11 where base-3 depends on syb
-- This means that if a package depends on base-3 and on syb, then they MUST
-- share the version of syb
--
-- * Package A relies on base-3 (which relies on base-4)
-- * Package B relies on base-4
-- * Package C relies on both A and B
-- * Package D relies on base-3 and on syb-2, which is not possible because
--     base-3 has a dependency on syb-1 (non-inheritance of the Base qualifier)
-- * Package E relies on base-4 and on syb-2, which is fine.
db12 :: ExampleDb
db12 =
  let base3 = exInst "base" 3 "base-3-inst" [base4, syb1]
      base4 = exInst "base" 4 "base-4-inst" []
      syb1  = exInst "syb" 1 "syb-1-inst" [base4]
  in [
      Left base3
    , Left base4
    , Left syb1
    , Right $ exAv "syb" 2 [ExFix "base" 4]
    , Right $ exAv "A" 1 [ExFix "base" 3, ExAny "syb"]
    , Right $ exAv "B" 1 [ExFix "base" 4, ExAny "syb"]
    , Right $ exAv "C" 1 [ExAny "A", ExAny "B"]
    , Right $ exAv "D" 1 [ExFix "base" 3, ExFix "syb" 2]
    , Right $ exAv "E" 1 [ExFix "base" 4, ExFix "syb" 2]
    ]

db13 :: ExampleDb
db13 = [
    Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 []
  , Right $ exAv "A" 3 []
  ]

-- | Database with some cycles
--
-- * Simplest non-trivial cycle: A -> B and B -> A
-- * There is a cycle C -> D -> C, but it can be broken by picking the
--   right flag assignment.
db14 :: ExampleDb
db14 = [
    Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "A"]
  , Right $ exAv "C" 1 [exFlag "flagC" [ExAny "D"] [ExAny "E"]]
  , Right $ exAv "D" 1 [ExAny "C"]
  , Right $ exAv "E" 1 []
  ]

-- | Cycles through setup dependencies
--
-- The first cycle is unsolvable: package A has a setup dependency on B,
-- B has a regular dependency on A, and we only have a single version available
-- for both.
--
-- The second cycle can be broken by picking different versions: package C-2.0
-- has a setup dependency on D, and D has a regular dependency on C-*. However,
-- version C-1.0 is already available (perhaps it didn't have this setup dep).
-- Thus, we should be able to break this cycle even if we are installing package
-- E, which explictly depends on C-2.0.
db15 :: ExampleDb
db15 = [
    -- First example (real cycle, no solution)
    Right $ exAv   "A" 1            []            `withSetupDeps` [ExAny "B"]
  , Right $ exAv   "B" 1            [ExAny "A"]
    -- Second example (cycle can be broken by picking versions carefully)
  , Left  $ exInst "C" 1 "C-1-inst" []
  , Right $ exAv   "C" 2            []            `withSetupDeps` [ExAny "D"]
  , Right $ exAv   "D" 1            [ExAny "C"  ]
  , Right $ exAv   "E" 1            [ExFix "C" 2]
  ]

-- | Check that the solver can backtrack after encountering the SIR
--
-- When A and B are installed as independent goals, the single instance
-- restriction prevents B from depending on C.  This database tests that the
-- solver can backtrack after encountering the single instance restriction and
-- choose the only valid flag assignment (-flagA +flagB):
--
-- > flagA flagB  B depends on
-- >  On    _     C-*
-- >  Off   On    E-*               <-- only valid flag assignment
-- >  Off   Off   D-2.0, C-*
--
-- Since A depends on C-* and D-1.0, and C-1.0 depends on any version of D,
-- we must build C-1.0 against D-1.0. Since B depends on D-2.0, we cannot have
-- C in the transitive closure of B's dependencies, because that would mean we
-- would need two instances of C: one built against D-1.0 and one built against
-- D-2.0.
db16 :: ExampleDb
db16 = [
    Right $ exAv "A" 1 [ExAny "C", ExFix "D" 1]
  , Right $ exAv "B" 1 [ ExFix "D" 2
                       , exFlag "flagA"
                             [ExAny "C"]
                             [exFlag "flagB"
                                 [ExAny "E"]
                                 [ExAny "C"]]]
  , Right $ exAv "C" 1 [ExAny "D"]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  , Right $ exAv "E" 1 []
  ]

dbExts1 :: ExampleDb
dbExts1 = [
    Right $ exAv "A" 1 [ExExt (EnableExtension RankNTypes)]
  , Right $ exAv "B" 1 [ExExt (EnableExtension CPP), ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "B"]
  , Right $ exAv "D" 1 [ExExt (DisableExtension CPP), ExAny "B"]
  , Right $ exAv "E" 1 [ExExt (UnknownExtension "custom"), ExAny "C"]
  ]

dbLangs1 :: ExampleDb
dbLangs1 = [
    Right $ exAv "A" 1 [ExLang Haskell2010]
  , Right $ exAv "B" 1 [ExLang Haskell98, ExAny "A"]
  , Right $ exAv "C" 1 [ExLang (UnknownLanguage "Haskell3000"), ExAny "B"]
  ]

-- | cabal must set enable-lib to false in order to avoid the unavailable
-- dependency. Flags are true by default. The flag choice causes "pkg" to
-- depend on "false-dep".
testBuildable :: String -> ExampleDependency -> TestTree
testBuildable testName unavailableDep =
    runTest $ mkTestExtLangPC (Just []) (Just []) [] db testName ["pkg"] expected
  where
    expected = Just [("false-dep", 1), ("pkg", 1)]
    db = [
        Right $ exAv "pkg" 1
            [ unavailableDep
            , ExFlag "enable-lib" (Buildable []) NotBuildable ]
         `withTest`
            ExTest "test" [exFlag "enable-lib"
                              [ExAny "true-dep"]
                              [ExAny "false-dep"]]
      , Right $ exAv "true-dep" 1 []
      , Right $ exAv "false-dep" 1 []
      ]

-- | cabal must choose -flag1 +flag2 for "pkg", which requires packages
-- "flag1-false" and "flag2-true".
dbBuildable1 :: ExampleDb
dbBuildable1 = [
    Right $ exAv "pkg" 1
        [ ExAny "unknown"
        , ExFlag "flag1" (Buildable []) NotBuildable
        , ExFlag "flag2" (Buildable []) NotBuildable]
     `withTests`
        [ ExTest "optional-test"
              [ ExAny "unknown"
              , ExFlag "flag1"
                    (Buildable [])
                    (Buildable [ExFlag "flag2" NotBuildable (Buildable [])])]
        , ExTest "test" [ exFlag "flag1" [ExAny "flag1-true"] [ExAny "flag1-false"]
                        , exFlag "flag2" [ExAny "flag2-true"] [ExAny "flag2-false"]]
        ]
  , Right $ exAv "flag1-true" 1 []
  , Right $ exAv "flag1-false" 1 []
  , Right $ exAv "flag2-true" 1 []
  , Right $ exAv "flag2-false" 1 []
  ]

-- | Package databases for testing @pkg-config@ dependencies.
dbPC1 :: ExampleDb
dbPC1 = [
    Right $ exAv "A" 1 [ExPkg ("pkgA", 1)]
  , Right $ exAv "B" 1 [ExPkg ("pkgB", 1), ExAny "A"]
  , Right $ exAv "B" 2 [ExPkg ("pkgB", 2), ExAny "A"]
  , Right $ exAv "C" 1 [ExAny "B"]
  ]

-- | cabal must pick B-2 to avoid the unknown dependency.
dbBuildable2 :: ExampleDb
dbBuildable2 = [
    Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "unknown"]
  , Right $ exAv "B" 2
        [ ExAny "unknown"
        , ExFlag "disable-lib" NotBuildable (Buildable [])
        ]
  , Right $ exAv "B" 3 [ExAny "unknown"]
  ]

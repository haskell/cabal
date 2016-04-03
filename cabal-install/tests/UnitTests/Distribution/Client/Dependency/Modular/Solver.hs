{-# LANGUAGE RecordWildCards #-}
module UnitTests.Distribution.Client.Dependency.Modular.Solver (tests)
       where

-- base
import Control.Monad
import Data.List (isInfixOf)

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
import UnitTests.Distribution.Client.Dependency.Modular.DSL
import UnitTests.Options

tests :: [TF.TestTree]
tests = [
      testGroup "Simple dependencies" [
          runTest $         mkTest db1 "alreadyInstalled"   ["A"]      (SolverSuccess [])
        , runTest $         mkTest db1 "installLatest"      ["B"]      (SolverSuccess [("B", 2)])
        , runTest $         mkTest db1 "simpleDep1"         ["C"]      (SolverSuccess [("B", 1), ("C", 1)])
        , runTest $         mkTest db1 "simpleDep2"         ["D"]      (SolverSuccess [("B", 2), ("D", 1)])
        , runTest $         mkTest db1 "failTwoVersions"    ["C", "D"] anySolverFailure
        , runTest $ indep $ mkTest db1 "indepTwoVersions"   ["C", "D"] (SolverSuccess [("B", 1), ("B", 2), ("C", 1), ("D", 1)])
        , runTest $ indep $ mkTest db1 "aliasWhenPossible1" ["C", "E"] (SolverSuccess [("B", 1), ("C", 1), ("E", 1)])
        , runTest $ indep $ mkTest db1 "aliasWhenPossible2" ["D", "E"] (SolverSuccess [("B", 2), ("D", 1), ("E", 1)])
        , runTest $ indep $ mkTest db2 "aliasWhenPossible3" ["C", "D"] (SolverSuccess [("A", 1), ("A", 2), ("B", 1), ("B", 2), ("C", 1), ("D", 1)])
        , runTest $         mkTest db1 "buildDepAgainstOld" ["F"]      (SolverSuccess [("B", 1), ("E", 1), ("F", 1)])
        , runTest $         mkTest db1 "buildDepAgainstNew" ["G"]      (SolverSuccess [("B", 2), ("E", 1), ("G", 1)])
        , runTest $ indep $ mkTest db1 "multipleInstances"  ["F", "G"] anySolverFailure
        , runTest $         mkTest db21 "unknownPackage1"   ["A"]      (SolverSuccess [("A", 1), ("B", 1)])
        , runTest $         mkTest db22 "unknownPackage2"   ["A"]      (SolverFailure (isInfixOf "unknown package: C"))
        ]
    , testGroup "Flagged dependencies" [
          runTest $         mkTest db3 "forceFlagOn"  ["C"]      (SolverSuccess [("A", 1), ("B", 1), ("C", 1)])
        , runTest $         mkTest db3 "forceFlagOff" ["D"]      (SolverSuccess [("A", 2), ("B", 1), ("D", 1)])
        , runTest $ indep $ mkTest db3 "linkFlags1"   ["C", "D"] anySolverFailure
        , runTest $ indep $ mkTest db4 "linkFlags2"   ["C", "D"] anySolverFailure
        , runTest $ indep $ mkTest db18 "linkFlags3"  ["A", "B"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2), ("F", 1)])
        ]
    , testGroup "Stanzas" [
          runTest $         mkTest db5 "simpleTest1" ["C"]      (SolverSuccess [("A", 2), ("C", 1)])
        , runTest $         mkTest db5 "simpleTest2" ["D"]      anySolverFailure
        , runTest $         mkTest db5 "simpleTest3" ["E"]      (SolverSuccess [("A", 1), ("E", 1)])
        , runTest $         mkTest db5 "simpleTest4" ["F"]      anySolverFailure -- TODO
        , runTest $         mkTest db5 "simpleTest5" ["G"]      (SolverSuccess [("A", 2), ("G", 1)])
        , runTest $         mkTest db5 "simpleTest6" ["E", "G"] anySolverFailure
        , runTest $ indep $ mkTest db5 "simpleTest7" ["E", "G"] (SolverSuccess [("A", 1), ("A", 2), ("E", 1), ("G", 1)])
        , runTest $         mkTest db6 "depsWithTests1" ["C"]      (SolverSuccess [("A", 1), ("B", 1), ("C", 1)])
        , runTest $ indep $ mkTest db6 "depsWithTests2" ["C", "D"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1)])
        ]
    , testGroup "Setup dependencies" [
          runTest $         mkTest db7  "setupDeps1" ["B"] (SolverSuccess [("A", 2), ("B", 1)])
        , runTest $         mkTest db7  "setupDeps2" ["C"] (SolverSuccess [("A", 2), ("C", 1)])
        , runTest $         mkTest db7  "setupDeps3" ["D"] (SolverSuccess [("A", 1), ("D", 1)])
        , runTest $         mkTest db7  "setupDeps4" ["E"] (SolverSuccess [("A", 1), ("A", 2), ("E", 1)])
        , runTest $         mkTest db7  "setupDeps5" ["F"] (SolverSuccess [("A", 1), ("A", 2), ("F", 1)])
        , runTest $         mkTest db8  "setupDeps6" ["C", "D"] (SolverSuccess [("A", 1), ("B", 1), ("B", 2), ("C", 1), ("D", 1)])
        , runTest $         mkTest db9  "setupDeps7" ["F", "G"] (SolverSuccess [("A", 1), ("B", 1), ("B",2 ), ("C", 1), ("D", 1), ("E", 1), ("E", 2), ("F", 1), ("G", 1)])
        , runTest $         mkTest db10 "setupDeps8" ["C"] (SolverSuccess [("C", 1)])
        , runTest $ indep $ mkTest dbSetupDeps "setupDeps9" ["A", "B"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2)])
        ]
    , testGroup "Base shim" [
          runTest $ mkTest db11 "baseShim1" ["A"] (SolverSuccess [("A", 1)])
        , runTest $ mkTest db12 "baseShim2" ["A"] (SolverSuccess [("A", 1)])
        , runTest $ mkTest db12 "baseShim3" ["B"] (SolverSuccess [("B", 1)])
        , runTest $ mkTest db12 "baseShim4" ["C"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1)])
        , runTest $ mkTest db12 "baseShim5" ["D"] anySolverFailure
        , runTest $ mkTest db12 "baseShim6" ["E"] (SolverSuccess [("E", 1), ("syb", 2)])
        ]
    , testGroup "Cycles" [
          runTest $ mkTest db14 "simpleCycle1"          ["A"]      anySolverFailure
        , runTest $ mkTest db14 "simpleCycle2"          ["A", "B"] anySolverFailure
        , runTest $ mkTest db14 "cycleWithFlagChoice1"  ["C"]      (SolverSuccess [("C", 1), ("E", 1)])
        , runTest $ mkTest db15 "cycleThroughSetupDep1" ["A"]      anySolverFailure
        , runTest $ mkTest db15 "cycleThroughSetupDep2" ["B"]      anySolverFailure
        , runTest $ mkTest db15 "cycleThroughSetupDep3" ["C"]      (SolverSuccess [("C", 2), ("D", 1)])
        , runTest $ mkTest db15 "cycleThroughSetupDep4" ["D"]      (SolverSuccess [("D", 1)])
        , runTest $ mkTest db15 "cycleThroughSetupDep5" ["E"]      (SolverSuccess [("C", 2), ("D", 1), ("E", 1)])
        ]
    , testGroup "Extensions" [
          runTest $ mkTestExts [EnableExtension CPP] dbExts1 "unsupported" ["A"] anySolverFailure
        , runTest $ mkTestExts [EnableExtension CPP] dbExts1 "unsupportedIndirect" ["B"] anySolverFailure
        , runTest $ mkTestExts [EnableExtension RankNTypes] dbExts1 "supported" ["A"] (SolverSuccess [("A",1)])
        , runTest $ mkTestExts (map EnableExtension [CPP,RankNTypes]) dbExts1 "supportedIndirect" ["C"] (SolverSuccess [("A",1),("B",1), ("C",1)])
        , runTest $ mkTestExts [EnableExtension CPP] dbExts1 "disabledExtension" ["D"] anySolverFailure
        , runTest $ mkTestExts (map EnableExtension [CPP,RankNTypes]) dbExts1 "disabledExtension" ["D"] anySolverFailure
        , runTest $ mkTestExts (UnknownExtension "custom" : map EnableExtension [CPP,RankNTypes]) dbExts1 "supportedUnknown" ["E"] (SolverSuccess [("A",1),("B",1),("C",1),("E",1)])
        ]
    , testGroup "Languages" [
          runTest $ mkTestLangs [Haskell98] dbLangs1 "unsupported" ["A"] anySolverFailure
        , runTest $ mkTestLangs [Haskell98,Haskell2010] dbLangs1 "supported" ["A"] (SolverSuccess [("A",1)])
        , runTest $ mkTestLangs [Haskell98] dbLangs1 "unsupportedIndirect" ["B"] anySolverFailure
        , runTest $ mkTestLangs [Haskell98, Haskell2010, UnknownLanguage "Haskell3000"] dbLangs1 "supportedUnknown" ["C"] (SolverSuccess [("A",1),("B",1),("C",1)])
        ]

     , testGroup "Soft Constraints" [
          runTest $ soft [ ExPref "A" $ mkvrThis 1]      $ mkTest db13 "selectPreferredVersionSimple" ["A"] (SolverSuccess [("A", 1)])
        , runTest $ soft [ ExPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionSimple2" ["A"] (SolverSuccess [("A", 2)])
        , runTest $ soft [ ExPref "A" $ mkvrOrEarlier 2
                         , ExPref "A" $ mkvrOrEarlier 1] $ mkTest db13 "selectPreferredVersionMultiple" ["A"] (SolverSuccess [("A", 1)])
        , runTest $ soft [ ExPref "A" $ mkvrOrEarlier 1
                         , ExPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionMultiple2" ["A"] (SolverSuccess [("A", 1)])
        , runTest $ soft [ ExPref "A" $ mkvrThis 1
                         , ExPref "A" $ mkvrThis 2] $ mkTest db13 "selectPreferredVersionMultiple3" ["A"] (SolverSuccess [("A", 2)])
        , runTest $ soft [ ExPref "A" $ mkvrThis 1
                         , ExPref "A" $ mkvrOrEarlier 2] $ mkTest db13 "selectPreferredVersionMultiple4" ["A"] (SolverSuccess [("A", 1)])
        ]
     , testGroup "Buildable Field" [
          testBuildable "avoid building component with unknown dependency" (ExAny "unknown")
        , testBuildable "avoid building component with unknown extension" (ExExt (UnknownExtension "unknown"))
        , testBuildable "avoid building component with unknown language" (ExLang (UnknownLanguage "unknown"))
        , runTest $ mkTest dbBuildable1 "choose flags that set buildable to false" ["pkg"] (SolverSuccess [("flag1-false", 1), ("flag2-true", 1), ("pkg", 1)])
        , runTest $ mkTest dbBuildable2 "choose version that sets buildable to false" ["A"] (SolverSuccess [("A", 1), ("B", 2)])
         ]
    , testGroup "Pkg-config dependencies" [
          runTest $ mkTestPCDepends [] dbPC1 "noPkgs" ["A"] anySolverFailure
        , runTest $ mkTestPCDepends [("pkgA", "0")] dbPC1 "tooOld" ["A"] anySolverFailure
        , runTest $ mkTestPCDepends [("pkgA", "1.0.0"), ("pkgB", "1.0.0")] dbPC1 "pruneNotFound" ["C"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1)])
        , runTest $ mkTestPCDepends [("pkgA", "1.0.0"), ("pkgB", "2.0.0")] dbPC1 "chooseNewest" ["C"] (SolverSuccess [("A", 1), ("B", 2), ("C", 1)])
        ]
    , testGroup "Independent goals" [
          runTest $ indep $ mkTest db16 "indepGoals1" ["A", "B"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2), ("E", 1)])
        , runTest $ indep $ mkTest db17 "indepGoals2" ["A", "B"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1)])
        , runTest $ indep $ mkTest db19 "indepGoals3" ["D", "E", "F"] anySolverFailure -- The target order is important.
        , runTest $ indep $ mkTest db20 "indepGoals4" ["C", "A", "B"] (SolverSuccess [("A", 1), ("B", 1), ("C", 1), ("D", 1), ("D", 2)])
        , runTest $ indep $ mkTest db23 "indepGoals5" ["X", "Y"] (SolverSuccess [("A", 1), ("A", 2), ("B", 1), ("C", 1), ("C", 2), ("X", 1), ("Y", 1)])
        , runTest $ indep $ mkTest db24 "indepGoals6" ["X", "Y"] (SolverSuccess [("A", 1), ("A", 2), ("B", 1), ("B", 2), ("X", 1), ("Y", 1)])
        ]
    ]
  where
    indep test      = test { testIndepGoals = True }
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
  , testResult         :: SolverResult
  , testIndepGoals     :: Bool
  , testSoftConstraints :: [ExPreference]
  , testDb             :: ExampleDb
  , testSupportedExts  :: Maybe [Extension]
  , testSupportedLangs :: Maybe [Language]
  , testPkgConfigDb    :: PkgConfigDb
  }

-- | Result of a solver test.
data SolverResult =
    SolverSuccess [(String, Int)]  -- ^ succeeds with given plan
  | SolverFailure (String -> Bool) -- ^ fails, and the error message satisfies the predicate

-- | Can be used for test cases where we just want to verify that
-- they fail, but do not care about the error message.
anySolverFailure :: SolverResult
anySolverFailure = SolverFailure (const True)

mkTest :: ExampleDb
       -> String
       -> [String]
       -> SolverResult
       -> SolverTest
mkTest = mkTestExtLangPC Nothing Nothing []

mkTestExts :: [Extension]
           -> ExampleDb
           -> String
           -> [String]
           -> SolverResult
           -> SolverTest
mkTestExts exts = mkTestExtLangPC (Just exts) Nothing []

mkTestLangs :: [Language]
            -> ExampleDb
            -> String
            -> [String]
            -> SolverResult
            -> SolverTest
mkTestLangs langs = mkTestExtLangPC Nothing (Just langs) []

mkTestPCDepends :: [(String, String)]
                -> ExampleDb
                -> String
                -> [String]
                -> SolverResult
                -> SolverTest
mkTestPCDepends pkgConfigDb = mkTestExtLangPC Nothing Nothing pkgConfigDb

mkTestExtLangPC :: Maybe [Extension]
                -> Maybe [Language]
                -> [(String, String)]
                -> ExampleDb
                -> String
                -> [String]
                -> SolverResult
                -> SolverTest
mkTestExtLangPC exts langs pkgConfigDb db label targets result = SolverTest {
    testLabel          = label
  , testTargets        = targets
  , testResult         = result
  , testIndepGoals     = False
  , testSoftConstraints = []
  , testDb             = db
  , testSupportedExts  = exts
  , testSupportedLangs = langs
  , testPkgConfigDb    = pkgConfigDbFromList pkgConfigDb
  }

runTest :: SolverTest -> TF.TestTree
runTest SolverTest{..} = askOption $ \(OptionShowSolverLog showSolverLog) ->
    testCase testLabel $ do
      let (_msgs, result) = exResolve testDb testSupportedExts testSupportedLangs
                            testPkgConfigDb testTargets testIndepGoals testSoftConstraints
      when showSolverLog $ mapM_ putStrLn _msgs
      case result of
        Left  err  -> assertBool ("Unexpected error:\n" ++ err) (check testResult err)
        Right plan -> assertEqual "" (toMaybe testResult) (Just (extractInstallPlan plan))
  where
    toMaybe :: SolverResult -> Maybe ([(String, Int)])
    toMaybe (SolverSuccess plan) = Just plan
    toMaybe (SolverFailure _   ) = Nothing

    check :: SolverResult -> (String -> Bool)
    check (SolverFailure f) = f
    check _                 = const False

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

-- | This database tests that a package's setup dependencies are correctly
-- linked when the package is linked. See pull request #3268.
--
-- When A and B are installed as independent goals, their dependencies on C must
-- be linked, due to the single instance restriction. Since C depends on D, 0.D
-- and 1.D must be linked. C also has a setup dependency on D, so 0.C-setup.D
-- and 1.C-setup.D must be linked. However, D's two link groups must remain
-- independent. The solver should be able to choose D-1 for C's library and D-2
-- for C's setup script.
dbSetupDeps :: ExampleDb
dbSetupDeps = [
    Right $ exAv "A" 1 [ExAny "C"]
  , Right $ exAv "B" 1 [ExAny "C"]
  , Right $ exAv "C" 1 [ExFix "D" 1] `withSetupDeps` [ExFix "D" 2]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
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

-- | Check that the solver can backtrack after encountering the SIR (issue #2843)
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

-- | This database checks that when the solver discovers a constraint on a
-- package's version after choosing to link that package, it can backtrack to
-- try alternative versions for the linked-to package. See pull request #3327.
--
-- When A and B are installed as independent goals, their dependencies on C
-- must be linked. Since C depends on D, A and B's dependencies on D must also
-- be linked. This test relies on the fact that the solver chooses D-2 for both
-- 0.D and 1.D before it encounters the test suites' constraints. The solver
-- must backtrack to try D-1 for both 0.D and 1.D.
db17 :: ExampleDb
db17 = [
    Right $ exAv "A" 1 [ExAny "C"] `withTest` ExTest "test" [ExFix "D" 1]
  , Right $ exAv "B" 1 [ExAny "C"] `withTest` ExTest "test" [ExFix "D" 1]
  , Right $ exAv "C" 1 [ExAny "D"]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  ]

-- | Issue #2834
-- When both A and B are installed as independent goals, their dependencies on
-- C must be linked. The only combination of C's flags that is consistent with
-- A and B's dependencies on D is -flagA +flagB. This database tests that the
-- solver can backtrack to find the right combination of flags (requiring F, but
-- not E or G) and apply it to both 0.C and 1.C.
--
-- > flagA flagB  C depends on
-- >  On    _     D-1, E-*
-- >  Off   On    F-*        <-- Only valid choice
-- >  Off   Off   D-2, G-*
--
-- The single instance restriction means we cannot have one instance of C
-- built against D-1 and one instance built against D-2; since A depends on
-- D-1, and B depends on C-2, it is therefore important that C cannot depend
-- on any version of D.
db18 :: ExampleDb
db18 = [
    Right $ exAv "A" 1 [ExAny "C", ExFix "D" 1]
  , Right $ exAv "B" 1 [ExAny "C", ExFix "D" 2]
  , Right $ exAv "C" 1 [exFlag "flagA"
                           [ExFix "D" 1, ExAny "E"]
                           [exFlag "flagB"
                               [ExAny "F"]
                               [ExFix "D" 2, ExAny "G"]]]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  , Right $ exAv "E" 1 []
  , Right $ exAv "F" 1 []
  , Right $ exAv "G" 1 []
  ]

-- | Tricky test case with independent goals (issue #2842)
--
-- Suppose we are installing D, E, and F as independent goals:
--
-- * D depends on A-* and C-1, requiring A-1 to be built against C-1
-- * E depends on B-* and C-2, requiring B-1 to be built against C-2
-- * F depends on A-* and B-*; this means we need A-1 and B-1 both to be built
--     against the same version of C, violating the single instance restriction.
--
-- We can visualize this DB as:
--
-- >    C-1   C-2
-- >    /|\   /|\
-- >   / | \ / | \
-- >  /  |  X  |  \
-- > |   | / \ |   |
-- > |   |/   \|   |
-- > |   +     +   |
-- > |   |     |   |
-- > |   A     B   |
-- >  \  |\   /|  /
-- >   \ | \ / | /
-- >    \|  V  |/
-- >     D  F  E
db19 :: ExampleDb
db19 = [
    Right $ exAv "A" 1 [ExAny "C"]
  , Right $ exAv "B" 1 [ExAny "C"]
  , Right $ exAv "C" 1 []
  , Right $ exAv "C" 2 []
  , Right $ exAv "D" 1 [ExAny "A", ExFix "C" 1]
  , Right $ exAv "E" 1 [ExAny "B", ExFix "C" 2]
  , Right $ exAv "F" 1 [ExAny "A", ExAny "B"]
  ]

-- | This database tests that the solver correctly backjumps when dependencies
-- of linked packages are not linked. It is an example where the conflict set
-- from enforcing the single instance restriction is not sufficient. See pull
-- request #3327.
--
-- When C, A, and B are installed as independent goals, the solver first
-- chooses 0.C-1 and 0.D-2. When choosing dependencies for A and B, it links
-- 1.D and 2.D to 0.D. Finally, the solver discovers the test's constraint on
-- D. It must backjump to try 1.D-1 and then link 2.D to 1.D.
db20 :: ExampleDb
db20 = [
    Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "D"] `withTest` ExTest "test" [ExFix "D" 1]
  , Right $ exAv "C" 1 [ExFix "D" 2]
  , Right $ exAv "D" 1 []
  , Right $ exAv "D" 2 []
  ]

-- | Test the trace messages that we get when a package refers to an unknown pkg
--
-- TODO: Currently we don't actually test the trace messages, and this particular
-- test still suceeds. The trace can only be verified by hand.
db21 :: ExampleDb
db21 = [
    Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "A" 2 [ExAny "C"] -- A-2.0 will be tried first, but C unknown
  , Right $ exAv "B" 1 []
  ]

-- | A variant of 'db21', which actually fails.
db22 :: ExampleDb
db22 = [
    Right $ exAv "A" 1 [ExAny "B"]
  , Right $ exAv "A" 2 [ExAny "C"]
  ]

-- | Database for (unsuccessfully) trying to expose a bug in the handling
-- of implied linking constraints. The question is whether an implied linking
-- constraint should only have the introducing package in its conflict set,
-- or also its link target.
--
-- It turns out that as long as the Single Instance Restriction is in place,
-- it does not matter, because there will aways be an option that is failing
-- due to the SIR, which contains the link target in its conflict set.
--
-- Even if the SIR is not in place, if there is a solution, one will always
-- be found, because without the SIR, linking is always optional, but never
-- necessary.
--
db23 :: ExampleDb
db23 = [
    Right $ exAv "X" 1 [ExFix "C" 2, ExAny "A"]
  , Right $ exAv "Y" 1 [ExFix "C" 1, ExFix "A" 2]
  , Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 [ExAny "B"]
  , Right $ exAv "B" 1 [ExAny "C"]
  , Right $ exAv "C" 1 []
  , Right $ exAv "C" 2 []
  ]

-- | A simplified version of 'db23'.
db24 :: ExampleDb
db24 = [
    Right $ exAv "X" 1 [ExFix "B" 2, ExAny "A"]
  , Right $ exAv "Y" 1 [ExFix "B" 1, ExFix "A" 2]
  , Right $ exAv "A" 1 []
  , Right $ exAv "A" 2 [ExAny "B"]
  , Right $ exAv "B" 1 []
  , Right $ exAv "B" 2 []
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
    expected = SolverSuccess [("false-dep", 1), ("pkg", 1)]
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

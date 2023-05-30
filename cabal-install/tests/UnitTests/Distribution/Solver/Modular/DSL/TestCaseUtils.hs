{-# LANGUAGE RecordWildCards #-}

-- | Utilities for creating HUnit test cases with the solver DSL.
module UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils
  ( SolverTest
  , SolverResult (..)
  , maxBackjumps
  , disableFineGrainedConflicts
  , minimizeConflictSet
  , independentGoals
  , preferOldest
  , allowBootLibInstalls
  , onlyConstrained
  , disableBackjumping
  , disableSolveExecutables
  , goalOrder
  , constraints
  , preferences
  , setVerbose
  , enableAllTests
  , solverSuccess
  , solverFailure
  , anySolverFailure
  , mkTest
  , mkTestExts
  , mkTestLangs
  , mkTestPCDepends
  , mkTestExtLangPC
  , runTest
  ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Data.List (elemIndex)

-- test-framework
import Test.Tasty as TF
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- Cabal
import qualified Distribution.PackageDescription as C
import Distribution.Verbosity
import Language.Haskell.Extension (Extension (..), Language (..))

-- cabal-install

import Distribution.Client.Dependency (foldProgress)
import qualified Distribution.Solver.Types.PackagePath as P
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb (..), pkgConfigDbFromList)
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.Variable
import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Options

maxBackjumps :: Maybe Int -> SolverTest -> SolverTest
maxBackjumps mbj test = test{testMaxBackjumps = mbj}

disableFineGrainedConflicts :: SolverTest -> SolverTest
disableFineGrainedConflicts test =
  test{testFineGrainedConflicts = FineGrainedConflicts False}

minimizeConflictSet :: SolverTest -> SolverTest
minimizeConflictSet test =
  test{testMinimizeConflictSet = MinimizeConflictSet True}

-- | Combinator to turn on --independent-goals behavior, i.e. solve
-- for the goals as if we were solving for each goal independently.
independentGoals :: SolverTest -> SolverTest
independentGoals test = test{testIndepGoals = IndependentGoals True}

-- | Combinator to turn on --prefer-oldest
preferOldest :: SolverTest -> SolverTest
preferOldest test = test{testPreferOldest = PreferOldest True}

allowBootLibInstalls :: SolverTest -> SolverTest
allowBootLibInstalls test =
  test{testAllowBootLibInstalls = AllowBootLibInstalls True}

onlyConstrained :: SolverTest -> SolverTest
onlyConstrained test =
  test{testOnlyConstrained = OnlyConstrainedAll}

disableBackjumping :: SolverTest -> SolverTest
disableBackjumping test =
  test{testEnableBackjumping = EnableBackjumping False}

disableSolveExecutables :: SolverTest -> SolverTest
disableSolveExecutables test =
  test{testSolveExecutables = SolveExecutables False}

goalOrder :: [ExampleVar] -> SolverTest -> SolverTest
goalOrder order test = test{testGoalOrder = Just order}

constraints :: [ExConstraint] -> SolverTest -> SolverTest
constraints cs test = test{testConstraints = cs}

preferences :: [ExPreference] -> SolverTest -> SolverTest
preferences prefs test = test{testSoftConstraints = prefs}

-- | Increase the solver's verbosity. This is necessary for test cases that
-- check the contents of the verbose log.
setVerbose :: SolverTest -> SolverTest
setVerbose test = test{testVerbosity = verbose}

enableAllTests :: SolverTest -> SolverTest
enableAllTests test = test{testEnableAllTests = EnableAllTests True}

{-------------------------------------------------------------------------------
  Solver tests
-------------------------------------------------------------------------------}

data SolverTest = SolverTest
  { testLabel :: String
  , testTargets :: [String]
  , testResult :: SolverResult
  , testMaxBackjumps :: Maybe Int
  , testFineGrainedConflicts :: FineGrainedConflicts
  , testMinimizeConflictSet :: MinimizeConflictSet
  , testIndepGoals :: IndependentGoals
  , testPreferOldest :: PreferOldest
  , testAllowBootLibInstalls :: AllowBootLibInstalls
  , testOnlyConstrained :: OnlyConstrained
  , testEnableBackjumping :: EnableBackjumping
  , testSolveExecutables :: SolveExecutables
  , testGoalOrder :: Maybe [ExampleVar]
  , testConstraints :: [ExConstraint]
  , testSoftConstraints :: [ExPreference]
  , testVerbosity :: Verbosity
  , testDb :: ExampleDb
  , testSupportedExts :: Maybe [Extension]
  , testSupportedLangs :: Maybe [Language]
  , testPkgConfigDb :: PkgConfigDb
  , testEnableAllTests :: EnableAllTests
  }

-- | Expected result of a solver test.
data SolverResult = SolverResult
  { resultLogPredicate :: [String] -> Bool
  -- ^ The solver's log should satisfy this predicate. Note that we also print
  -- the log, so evaluating a large log here can cause a space leak.
  , resultErrorMsgPredicateOrPlan :: Either (String -> Bool) [(String, Int)]
  -- ^ Fails with an error message satisfying the predicate, or succeeds with
  -- the given plan.
  }

solverSuccess :: [(String, Int)] -> SolverResult
solverSuccess = SolverResult (const True) . Right

solverFailure :: (String -> Bool) -> SolverResult
solverFailure = SolverResult (const True) . Left

-- | Can be used for test cases where we just want to verify that
-- they fail, but do not care about the error message.
anySolverFailure :: SolverResult
anySolverFailure = solverFailure (const True)

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
-- See 'UnitTests.Distribution.Solver.Modular.DSL' for how
-- to construct an 'ExampleDb', as well as definitions of 'db1' etc.
-- in this file.
mkTest
  :: ExampleDb
  -> String
  -> [String]
  -> SolverResult
  -> SolverTest
mkTest = mkTestExtLangPC Nothing Nothing (Just [])

mkTestExts
  :: [Extension]
  -> ExampleDb
  -> String
  -> [String]
  -> SolverResult
  -> SolverTest
mkTestExts exts = mkTestExtLangPC (Just exts) Nothing (Just [])

mkTestLangs
  :: [Language]
  -> ExampleDb
  -> String
  -> [String]
  -> SolverResult
  -> SolverTest
mkTestLangs langs = mkTestExtLangPC Nothing (Just langs) (Just [])

mkTestPCDepends
  :: Maybe [(String, String)]
  -> ExampleDb
  -> String
  -> [String]
  -> SolverResult
  -> SolverTest
mkTestPCDepends mPkgConfigDb = mkTestExtLangPC Nothing Nothing mPkgConfigDb

mkTestExtLangPC
  :: Maybe [Extension]
  -> Maybe [Language]
  -> Maybe [(String, String)]
  -> ExampleDb
  -> String
  -> [String]
  -> SolverResult
  -> SolverTest
mkTestExtLangPC exts langs mPkgConfigDb db label targets result =
  SolverTest
    { testLabel = label
    , testTargets = targets
    , testResult = result
    , testMaxBackjumps = Nothing
    , testFineGrainedConflicts = FineGrainedConflicts True
    , testMinimizeConflictSet = MinimizeConflictSet False
    , testIndepGoals = IndependentGoals False
    , testPreferOldest = PreferOldest False
    , testAllowBootLibInstalls = AllowBootLibInstalls False
    , testOnlyConstrained = OnlyConstrainedNone
    , testEnableBackjumping = EnableBackjumping True
    , testSolveExecutables = SolveExecutables True
    , testGoalOrder = Nothing
    , testConstraints = []
    , testSoftConstraints = []
    , testVerbosity = normal
    , testDb = db
    , testSupportedExts = exts
    , testSupportedLangs = langs
    , testPkgConfigDb = maybe NoPkgConfigDb pkgConfigDbFromList mPkgConfigDb
    , testEnableAllTests = EnableAllTests False
    }

runTest :: SolverTest -> TF.TestTree
runTest SolverTest{..} = askOption $ \(OptionShowSolverLog showSolverLog) ->
  testCase testLabel $ do
    let progress =
          exResolve
            testDb
            testSupportedExts
            testSupportedLangs
            testPkgConfigDb
            testTargets
            testMaxBackjumps
            (CountConflicts True)
            testFineGrainedConflicts
            testMinimizeConflictSet
            testIndepGoals
            testPreferOldest
            (ReorderGoals False)
            testAllowBootLibInstalls
            testOnlyConstrained
            testEnableBackjumping
            testSolveExecutables
            (sortGoals <$> testGoalOrder)
            testConstraints
            testSoftConstraints
            testVerbosity
            testEnableAllTests
        printMsg msg = when showSolverLog $ putStrLn msg
        msgs = foldProgress (:) (const []) (const []) progress
    assertBool ("Unexpected solver log:\n" ++ unlines msgs) $
      resultLogPredicate testResult $
        concatMap lines msgs
    result <- foldProgress ((>>) . printMsg) (return . Left) (return . Right) progress
    case result of
      Left err ->
        assertBool
          ("Unexpected error:\n" ++ err)
          (checkErrorMsg testResult err)
      Right plan -> assertEqual "" (toMaybe testResult) (Just (extractInstallPlan plan))
  where
    toMaybe :: SolverResult -> Maybe [(String, Int)]
    toMaybe = either (const Nothing) Just . resultErrorMsgPredicateOrPlan

    checkErrorMsg :: SolverResult -> String -> Bool
    checkErrorMsg result msg =
      case resultErrorMsgPredicateOrPlan result of
        Left f -> f msg
        Right _ -> False

    sortGoals
      :: [ExampleVar]
      -> Variable P.QPN
      -> Variable P.QPN
      -> Ordering
    sortGoals = orderFromList . map toVariable

    -- Sort elements in the list ahead of elements not in the list. Otherwise,
    -- follow the order in the list.
    orderFromList :: Eq a => [a] -> a -> a -> Ordering
    orderFromList xs =
      comparing $ \x -> let i = elemIndex x xs in (isNothing i, i)

    toVariable :: ExampleVar -> Variable P.QPN
    toVariable (P q pn) = PackageVar (toQPN q pn)
    toVariable (F q pn fn) = FlagVar (toQPN q pn) (C.mkFlagName fn)
    toVariable (S q pn stanza) = StanzaVar (toQPN q pn) stanza

    toQPN :: ExampleQualifier -> ExamplePkgName -> P.QPN
    toQPN q pn = P.Q pp (C.mkPackageName pn)
      where
        pp = case q of
          QualNone -> P.PackagePath P.DefaultNamespace P.QualToplevel
          QualIndep p ->
            P.PackagePath
              (P.Independent $ C.mkPackageName p)
              P.QualToplevel
          QualSetup s ->
            P.PackagePath
              P.DefaultNamespace
              (P.QualSetup (C.mkPackageName s))
          QualIndepSetup p s ->
            P.PackagePath
              (P.Independent $ C.mkPackageName p)
              (P.QualSetup (C.mkPackageName s))
          QualExe p1 p2 ->
            P.PackagePath
              P.DefaultNamespace
              (P.QualExe (C.mkPackageName p1) (C.mkPackageName p2))

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Solver.Modular.QuickCheck (tests) where

import Control.DeepSeq (NFData, force)
import Control.Monad (foldM)
import Data.Either (lefts)
import Data.Function (on)
import Data.List (groupBy, isInfixOf, nub, nubBy, sort)
import Data.Maybe (isJust)
import GHC.Generics (Generic)

import Control.Applicative ((<|>))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid)
#endif

import Text.Show.Pretty (parseValue, valToStr)

import Test.Tasty (TestTree)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.QuickCheck

import Distribution.Client.Dependency.Types
         ( Solver(..) )
import Distribution.Client.Setup (defaultMaxBackjumps)

import           Distribution.Types.UnqualComponentName

import qualified Distribution.Solver.Types.ComponentDeps as CD
import           Distribution.Solver.Types.ComponentDeps
                   ( Component(..), ComponentDep, ComponentDeps )
import           Distribution.Solver.Types.PkgConfigDb
                   (pkgConfigDbFromList)
import           Distribution.Solver.Types.Settings

import UnitTests.Distribution.Solver.Modular.DSL

tests :: [TestTree]
tests = [
      -- This test checks that certain solver parameters do not affect the
      -- existence of a solution. It runs the solver twice, and only sets those
      -- parameters on the second run. The test also applies parameters that
      -- can affect the existence of a solution to both runs.
      testProperty "target order and --reorder-goals do not affect solvability" $
          \(SolverTest db targets) targetOrder reorderGoals indepGoals solver ->
            let r1 = solve' (ReorderGoals False) targets  db
                r2 = solve' reorderGoals         targets2 db
                solve' reorder = solve (EnableBackjumping True) Nothing reorder
                                       indepGoals (FindBestSolution False) solver
                targets2 = case targetOrder of
                             SameOrder -> targets
                             ReverseOrder -> reverse targets
            in counterexample (showResults r1 r2) $
               noneReachedBackjumpLimit [r1, r2] ==>
               isRight (resultPlan r1) === isRight (resultPlan r2)

    -- This test currently fails because goal order affects the number of
    -- available link choices and their scores. It also fails because it
    -- needs to filter out all runs that reach the backjump limit, not just
    -- runs that don't find any solution.
    , ignoreTest $
      testProperty "target order and --reorder-goals do not affect best score" $
          \(SolverTest db targets) targetOrder reorderGoals indepGoals ->
            let r1 = solve' (ReorderGoals False) targets  db
                r2 = solve' reorderGoals         targets2 db
                solve' reorder = solve (EnableBackjumping True) Nothing reorder
                                       indepGoals (FindBestSolution True)
                                       Modular
                targets2 = case targetOrder of
                             SameOrder -> targets
                             ReverseOrder -> reverse targets
            in counterexample (showResults r1 r2) $
               noneReachedBackjumpLimit [r1, r2] ==>
               compareScores (\s1 s2 -> abs (s1 - s2) < 0.001) r1 r2

    , testProperty "install plan score is non-negative" $
          \(SolverTest db targets) reorder indepGoals findBest ->
            let result = solve (EnableBackjumping True) Nothing reorder
                               indepGoals findBest Modular targets db
            in counterexample (showResult 1 result) $
               maybe True (>= 0) (maybeScore result)

    , testProperty "best score before backjump limit <= first score" $
          \(SolverTest db targets) reorderGoals indepGoals ->
            let r1 = solve' (FindBestSolution True)  targets db
                r2 = solve' (FindBestSolution False) targets db
                solve' findBest = solve (EnableBackjumping True) Nothing
                                        reorderGoals indepGoals
                                        findBest Modular
            in counterexample (showResults r1 r2) $
               r1 `isBetterThan` r2

    , testProperty "maxBackjumps1 >= maxBackjumps2  =>  score1 <= score2" $
          \(SolverTest db targets) reorderGoals indepGoals
               (NonNegative (Small x)) (NonNegative (Small y)) ->
            let r1 = solve' (10 * max x y) targets db
                r2 = solve' (10 * min x y) targets db
                solve' mbj = solve (EnableBackjumping True) (Just mbj)
                                   reorderGoals indepGoals
                                   (FindBestSolution True) Modular
            in counterexample (showResults r1 r2) $
               r1 `isBetterThan` r2

    , testProperty
          "solvable without --independent-goals => solvable with --independent-goals" $
          \(SolverTest db targets) reorderGoals solver ->
            let r1 = solve' (IndependentGoals False) targets db
                r2 = solve' (IndependentGoals True)  targets db
                solve' indep = solve (EnableBackjumping True)
                                     Nothing reorderGoals indep
                                     (FindBestSolution False) solver
             in counterexample (showResults r1 r2) $
                noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) `implies` isRight (resultPlan r2)

    , testProperty "backjumping does not affect solvability" $
          \(SolverTest db targets) reorderGoals indepGoals ->
            let r1 = solve' (EnableBackjumping True)  targets db
                r2 = solve' (EnableBackjumping False) targets db
                solve' enableBj = solve enableBj Nothing reorderGoals indepGoals
                                        (FindBestSolution False) Modular
             in counterexample (showResults r1 r2) $
                noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
    ]
  where
    noneReachedBackjumpLimit :: [Result] -> Bool
    noneReachedBackjumpLimit =
        not . any (\r -> resultPlan r == Left BackjumpLimitReached)

    isBetterThan :: Result -> Result -> Bool
    isBetterThan = cmp `on` maybeScore
      where
        cmp :: Maybe InstallPlanScore -> Maybe InstallPlanScore -> Bool
        cmp _         Nothing   = True
        cmp (Just s1) (Just s2) = s1 < s2 + 0.001 -- lower is better
        cmp _         _         = False

    compareScores :: (InstallPlanScore -> InstallPlanScore -> Bool)
                  -> Result -> Result -> Bool
    compareScores f = cmp `on` maybeScore
      where
        cmp :: Maybe InstallPlanScore -> Maybe InstallPlanScore -> Bool
        cmp Nothing   Nothing   = True
        cmp (Just s1) (Just s2) = f s1 s2
        cmp _         _         = False

    maybeScore :: Result -> Maybe InstallPlanScore
    maybeScore = either (const Nothing) (Just . snd) . resultPlan

    showResults :: Result -> Result -> String
    showResults r1 r2 = showResult 1 r1 ++ showResult 2 r2

    showResult :: Int -> Result -> String
    showResult n result =
        unlines $ ["", "Run " ++ show n ++ ":"]
               ++ resultLog result
               ++ ["result: " ++ show (resultPlan result)]

    implies :: Bool -> Bool -> Bool
    implies x y = not x || y

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _         = False

solve :: EnableBackjumping -> Maybe Int -> ReorderGoals -> IndependentGoals
      -> FindBestSolution -> Solver -> [PN] -> TestDb -> Result
solve enableBj mbj reorder indep findBest solver targets (TestDb db) =
  let (lg, result) =
        runProgress $ exResolve db Nothing Nothing
                  (pkgConfigDbFromList [])
                  (map unPN targets)
                  solver
                  -- The backjump limit prevents individual tests from using
                  -- too much time and memory.
                  (mbj <|> Just defaultMaxBackjumps)
                  indep reorder (AllowBootLibInstalls False) findBest enableBj Nothing [] []
                  (EnableAllTests True)

      failure :: String -> Failure
      failure msg
        | "Backjump limit reached" `isInfixOf` msg = BackjumpLimitReached
        | otherwise                                = OtherFailure
  in Result {
       resultLog = lg
     , resultPlan =
         -- Force the result so that we check for internal errors when we check
         -- for success or failure. See D.C.Dependency.validateSolverResult.
         force $ either (Left . failure) (Right . extractInstallPlan) result
     }

-- | How to modify the order of the input targets.
data TargetOrder = SameOrder | ReverseOrder
  deriving Show

instance Arbitrary TargetOrder where
  arbitrary = elements [SameOrder, ReverseOrder]

  shrink SameOrder = []
  shrink ReverseOrder = [SameOrder]

data Result = Result {
    resultLog :: [String]
  , resultPlan :: Either Failure
                    ([(ExamplePkgName, ExamplePkgVersion)], InstallPlanScore)
  }

data Failure = BackjumpLimitReached | OtherFailure
  deriving (Eq, Generic, Show)

instance NFData Failure
instance NFData InstallPlanScore

-- | Package name.
newtype PN = PN { unPN :: String }
  deriving (Eq, Ord, Show)

instance Arbitrary PN where
  arbitrary = PN <$> elements ("base" : [[pn] | pn <- ['A'..'G']])

-- | Package version.
newtype PV = PV { unPV :: Int }
  deriving (Eq, Ord, Show)

instance Arbitrary PV where
  arbitrary = PV <$> elements [1..10]

type TestPackage = Either ExampleInstalled ExampleAvailable

getName :: TestPackage -> PN
getName = PN . either exInstName exAvName

getVersion :: TestPackage -> PV
getVersion = PV . either exInstVersion exAvVersion

data SolverTest = SolverTest {
    testDb :: TestDb
  , testTargets :: [PN]
  }

-- | Pretty-print the test when quickcheck calls 'show'.
instance Show SolverTest where
  show test =
    let str = "SolverTest {testDb = " ++ show (testDb test)
                     ++ ", testTargets = " ++ show (testTargets test) ++ "}"
    in maybe str valToStr $ parseValue str

instance Arbitrary SolverTest where
  arbitrary = do
    db <- arbitrary
    let pkgs = nub $ map getName (unTestDb db)
    Positive n <- arbitrary
    targets <- randomSubset n pkgs
    return (SolverTest db targets)

  shrink test =
         [test { testDb = db } | db <- shrink (testDb test)]
      ++ [test { testTargets = targets } | targets <- shrink (testTargets test)]

-- | Collection of source and installed packages.
newtype TestDb = TestDb { unTestDb :: ExampleDb }
  deriving Show

instance Arbitrary TestDb where
  arbitrary = do
      -- Avoid cyclic dependencies by grouping packages by name and only
      -- allowing each package to depend on packages in the groups before it.
      groupedPkgs <- shuffle . groupBy ((==) `on` fst) . nub . sort =<<
                     boundedListOf 10 arbitrary
      db <- foldM nextPkgs (TestDb []) groupedPkgs
      TestDb <$> shuffle (unTestDb db)
    where
      nextPkgs :: TestDb -> [(PN, PV)] -> Gen TestDb
      nextPkgs db pkgs = TestDb . (++ unTestDb db) <$> mapM (nextPkg db) pkgs

      nextPkg :: TestDb -> (PN, PV) -> Gen TestPackage
      nextPkg db (pn, v) = do
        installed <- arbitrary
        if installed
        then Left <$> arbitraryExInst pn v (lefts $ unTestDb db)
        else Right <$> arbitraryExAv pn v db

  shrink (TestDb pkgs) = map TestDb $ shrink pkgs

arbitraryExAv :: PN -> PV -> TestDb -> Gen ExampleAvailable
arbitraryExAv pn v db =
    (\cds -> ExAv (unPN pn) (unPV v) cds []) <$> arbitraryComponentDeps db

arbitraryExInst :: PN -> PV -> [ExampleInstalled] -> Gen ExampleInstalled
arbitraryExInst pn v pkgs = do
  hash <- vectorOf 10 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  numDeps <- min 3 <$> arbitrary
  deps <- randomSubset numDeps pkgs
  return $ ExInst (unPN pn) (unPV v) hash (map exInstHash deps)

arbitraryComponentDeps :: TestDb -> Gen (ComponentDeps [ExampleDependency])
arbitraryComponentDeps (TestDb []) = return $ CD.fromList []
arbitraryComponentDeps db =
    -- dedupComponentNames removes components with duplicate names, for example,
    -- 'ComponentExe x' and 'ComponentTest x', and then CD.fromList combines
    -- duplicate unnamed components.
    CD.fromList . dedupComponentNames <$>
    boundedListOf 5 (arbitraryComponentDep db)
  where
    dedupComponentNames =
        nubBy ((\x y -> isJust x && isJust y && x == y) `on` componentName . fst)

    componentName :: Component -> Maybe UnqualComponentName
    componentName ComponentLib        = Nothing
    componentName ComponentSetup      = Nothing
    componentName (ComponentSubLib n) = Just n
    componentName (ComponentFLib   n) = Just n
    componentName (ComponentExe    n) = Just n
    componentName (ComponentTest   n) = Just n
    componentName (ComponentBench  n) = Just n

arbitraryComponentDep :: TestDb -> Gen (ComponentDep [ExampleDependency])
arbitraryComponentDep db = do
  comp <- arbitrary
  deps <- case comp of
            ComponentSetup -> smallListOf (arbitraryExDep db SetupDep)
            _              -> boundedListOf 5 (arbitraryExDep db NonSetupDep)
  return (comp, deps)

-- | Location of an 'ExampleDependency'. It determines which values are valid.
data ExDepLocation = SetupDep | NonSetupDep

arbitraryExDep :: TestDb -> ExDepLocation -> Gen ExampleDependency
arbitraryExDep db@(TestDb pkgs) level =
  let flag = ExFlagged <$> arbitraryFlagName
                       <*> arbitraryDeps db
                       <*> arbitraryDeps db
      other =
          -- Package checks require dependencies on "base" to have bounds.
        let notBase = filter ((/= PN "base") . getName) pkgs
        in  [ExAny . unPN <$> elements (map getName notBase) | not (null notBase)]
         ++ [
              -- existing version
              let fixed pkg = ExFix (unPN $ getName pkg) (unPV $ getVersion pkg)
              in fixed <$> elements pkgs

              -- random version of an existing package
            , ExFix . unPN . getName <$> elements pkgs <*> (unPV <$> arbitrary)
            ]
  in oneof $
      case level of
        NonSetupDep -> flag : other
        SetupDep -> other

arbitraryDeps :: TestDb -> Gen Dependencies
arbitraryDeps db = frequency
    [ (1, return NotBuildable)
    , (20, Buildable <$> smallListOf (arbitraryExDep db NonSetupDep))
    ]

arbitraryFlagName :: Gen String
arbitraryFlagName = (:[]) <$> elements ['A'..'E']

instance Arbitrary ReorderGoals where
  arbitrary = ReorderGoals <$> arbitrary

  shrink (ReorderGoals reorder) = [ReorderGoals False | reorder]

instance Arbitrary IndependentGoals where
  arbitrary = IndependentGoals <$> arbitrary

  shrink (IndependentGoals indep) = [IndependentGoals False | indep]

instance Arbitrary FindBestSolution where
  arbitrary = FindBestSolution <$> arbitrary

  shrink (FindBestSolution indep) = [FindBestSolution False | indep]

instance Arbitrary Solver where
  arbitrary = return Modular

  shrink Modular = []

instance Arbitrary UnqualComponentName where
  arbitrary = mkUnqualComponentName <$> (:[]) <$> elements "ABC"

instance Arbitrary Component where
  arbitrary = oneof [ return ComponentLib
                    , ComponentSubLib <$> arbitrary
                    , ComponentExe <$> arbitrary
                    , ComponentFLib <$> arbitrary
                    , ComponentTest <$> arbitrary
                    , ComponentBench <$> arbitrary
                    , return ComponentSetup
                    ]

  shrink ComponentLib = []
  shrink _ = [ComponentLib]

instance Arbitrary ExampleInstalled where
  arbitrary = error "arbitrary not implemented: ExampleInstalled"

  shrink ei = [ ei { exInstBuildAgainst = deps }
              | deps <- shrinkList shrinkNothing (exInstBuildAgainst ei)]

instance Arbitrary ExampleAvailable where
  arbitrary = error "arbitrary not implemented: ExampleAvailable"

  shrink ea = [ea { exAvDeps = deps } | deps <- shrink (exAvDeps ea)]

instance (Arbitrary a, Monoid a) => Arbitrary (ComponentDeps a) where
  arbitrary = error "arbitrary not implemented: ComponentDeps"

  shrink = map CD.fromList . shrink . CD.toList

instance Arbitrary ExampleDependency where
  arbitrary = error "arbitrary not implemented: ExampleDependency"

  shrink (ExAny _) = []
  shrink (ExFix "base" _) = [] -- preserve bounds on base
  shrink (ExFix pn _) = [ExAny pn]
  shrink (ExFlagged flag th el) =
         deps th ++ deps el
      ++ [ExFlagged flag th' el | th' <- shrink th]
      ++ [ExFlagged flag th el' | el' <- shrink el]
    where
      deps NotBuildable = []
      deps (Buildable ds) = ds
  shrink dep = error $ "Dependency not handled: " ++ show dep

instance Arbitrary Dependencies where
  arbitrary = error "arbitrary not implemented: Dependencies"

  shrink NotBuildable = [Buildable []]
  shrink (Buildable deps) = map Buildable (shrink deps)

randomSubset :: Int -> [a] -> Gen [a]
randomSubset n xs = take n <$> shuffle xs

boundedListOf :: Int -> Gen a -> Gen [a]
boundedListOf n gen = take n <$> listOf gen

-- | Generates lists with average length less than 1.
smallListOf :: Gen a -> Gen [a]
smallListOf gen =
    frequency [ (fr, vectorOf n gen)
              | (fr, n) <- [(3, 0), (5, 1), (2, 2)]]

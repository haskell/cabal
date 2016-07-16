{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Solver.Modular.QuickCheck (tests) where

import Control.Monad (foldM)
import Data.Either (lefts)
import Data.Function (on)
import Data.List (groupBy, isInfixOf, nub, sort)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid)
#endif

import Text.Show.Pretty (parseValue, valToStr)

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck

import Distribution.Client.Dependency.Types
         ( Solver(..) )
import Distribution.Client.Setup (defaultMaxBackjumps)

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
                solve' reorder = solve (EnableBackjumping True) reorder
                                       indepGoals solver
                targets2 = case targetOrder of
                             SameOrder -> targets
                             ReverseOrder -> reverse targets
            in counterexample (showResults r1 r2) $
               noneReachedBackjumpLimit [r1, r2] ==>
               isRight (resultPlan r1) === isRight (resultPlan r2)

    , testProperty
          "solvable without --independent-goals => solvable with --independent-goals" $
          \(SolverTest db targets) reorderGoals solver ->
            let r1 = solve' (IndependentGoals False) targets db
                r2 = solve' (IndependentGoals True)  targets db
                solve' indep = solve (EnableBackjumping True)
                                     reorderGoals indep solver
             in counterexample (showResults r1 r2) $
                noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) `implies` isRight (resultPlan r2)

    , testProperty "backjumping does not affect solvability" $
          \(SolverTest db targets) reorderGoals indepGoals ->
            let r1 = solve' (EnableBackjumping True)  targets db
                r2 = solve' (EnableBackjumping False) targets db
                solve' enableBj = solve enableBj reorderGoals indepGoals Modular
             in counterexample (showResults r1 r2) $
                noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
    ]
  where
    noneReachedBackjumpLimit :: [Result] -> Bool
    noneReachedBackjumpLimit =
        not . any (\r -> resultPlan r == Left BackjumpLimitReached)

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

solve :: EnableBackjumping -> ReorderGoals -> IndependentGoals
      -> Solver -> [PN] -> TestDb -> Result
solve enableBj reorder indep solver targets (TestDb db) =
  let (lg, result) =
        runProgress $ exResolve db Nothing Nothing
                  (pkgConfigDbFromList [])
                  (map unPN targets)
                  solver
                  -- The backjump limit prevents individual tests from using
                  -- too much time and memory.
                  (Just defaultMaxBackjumps)
                  indep reorder enableBj Nothing []

      failure :: String -> Failure
      failure msg
        | "Backjump limit reached" `isInfixOf` msg = BackjumpLimitReached
        | otherwise                                = OtherFailure
  in Result {
       resultLog = lg
     , resultPlan = either (Left . failure) (Right . extractInstallPlan) result
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
  , resultPlan :: Either Failure [(ExamplePkgName, ExamplePkgVersion)]
  }

data Failure = BackjumpLimitReached | OtherFailure
  deriving (Eq, Show)

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
    ExAv (unPN pn) (unPV v) <$> arbitraryComponentDeps db

arbitraryExInst :: PN -> PV -> [ExampleInstalled] -> Gen ExampleInstalled
arbitraryExInst pn v pkgs = do
  hash <- vectorOf 10 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  numDeps <- min 3 <$> arbitrary
  deps <- randomSubset numDeps pkgs
  return $ ExInst (unPN pn) (unPV v) hash (map exInstHash deps)

arbitraryComponentDeps :: TestDb -> Gen (ComponentDeps [ExampleDependency])
arbitraryComponentDeps (TestDb []) = return $ CD.fromList []
arbitraryComponentDeps db =
    -- CD.fromList combines duplicate components.
    CD.fromList <$> boundedListOf 3 (arbitraryComponentDep db)

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
  let flag = ExFlag <$> arbitraryFlagName
                    <*> arbitraryDeps db
                    <*> arbitraryDeps db
      other = [
            ExAny . unPN <$> elements (map getName pkgs)

          -- existing version
          , let fixed pkg = ExFix (unPN $ getName pkg) (unPV $ getVersion pkg)
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

arbitraryComponentName :: Gen String
arbitraryComponentName = (:[]) <$> elements "ABC"

instance Arbitrary ReorderGoals where
  arbitrary = ReorderGoals <$> arbitrary

  shrink (ReorderGoals reorder) = [ReorderGoals False | reorder]

instance Arbitrary IndependentGoals where
  arbitrary = IndependentGoals <$> arbitrary

  shrink (IndependentGoals indep) = [IndependentGoals False | indep]

instance Arbitrary Solver where
  arbitrary = frequency [ (1, return TopDown)
                        , (5, return Modular) ]

  shrink Modular = []
  shrink TopDown = [Modular]

instance Arbitrary Component where
  arbitrary = oneof [ ComponentLib <$> arbitraryComponentName
                    , ComponentExe <$> arbitraryComponentName
                    , ComponentTest <$> arbitraryComponentName
                    , ComponentBench <$> arbitraryComponentName
                    , return ComponentSetup
                    ]

  shrink (ComponentLib "") = []
  shrink _ = [ComponentLib ""]

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
  shrink (ExFix pn _) = [ExAny pn]
  shrink (ExFlag flag th el) =
         deps th ++ deps el
      ++ [ExFlag flag th' el | th' <- shrink th]
      ++ [ExFlag flag th el' | el' <- shrink el]
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

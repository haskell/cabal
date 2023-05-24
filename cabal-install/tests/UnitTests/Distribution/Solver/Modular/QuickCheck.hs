{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module UnitTests.Distribution.Solver.Modular.QuickCheck (tests) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Control.Arrow ((&&&))
import Data.Either (lefts)
import Data.Hashable (Hashable (..))
import Data.List (groupBy, isInfixOf)

import Text.Show.Pretty (parseValue, valToStr)

import Test.QuickCheck (Arbitrary (..), Blind (..), Gen, Positive (..), counterexample, elements, frequency, listOf, oneof, shrinkList, shrinkNothing, shuffle, sublistOf, vectorOf, (===), (==>))
import Test.QuickCheck.Instances.Cabal ()
import Test.Tasty (TestTree)

import Distribution.Types.Flag (FlagName)
import Distribution.Utils.ShortText (ShortText)

import Distribution.Client.Setup (defaultMaxBackjumps)

import Distribution.Types.LibraryVisibility
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName

import Distribution.Solver.Types.ComponentDeps
  ( Component (..)
  , ComponentDep
  , ComponentDeps
  )
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackageConstraint
import qualified Distribution.Solver.Types.PackagePath as P
import Distribution.Solver.Types.PkgConfigDb
  ( pkgConfigDbFromList
  )
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.Variable
import Distribution.Verbosity
import Distribution.Version

import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.QuickCheck.Utils
  ( testPropertyWithSeed
  )

tests :: [TestTree]
tests =
  [ -- This test checks that certain solver parameters do not affect the
    -- existence of a solution. It runs the solver twice, and only sets those
    -- parameters on the second run. The test also applies parameters that
    -- can affect the existence of a solution to both runs.
    testPropertyWithSeed "target and goal order do not affect solvability" $
      \test targetOrder mGoalOrder1 mGoalOrder2 indepGoals ->
        let r1 = solve' mGoalOrder1 test
            r2 = solve' mGoalOrder2 test{testTargets = targets2}
            solve' goalOrder =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                (ReorderGoals False)
                (CountConflicts True)
                indepGoals
                (PreferOldest False)
                (getBlind <$> goalOrder)
            targets = testTargets test
            targets2 = case targetOrder of
              SameOrder -> targets
              ReverseOrder -> reverse targets
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , testPropertyWithSeed
      "solvable without --independent-goals => solvable with --independent-goals"
      $ \test reorderGoals ->
        let r1 = solve' (IndependentGoals False) test
            r2 = solve' (IndependentGoals True) test
            solve' indep =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                reorderGoals
                (CountConflicts True)
                indep
                (PreferOldest False)
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) `implies` isRight (resultPlan r2)
  , testPropertyWithSeed "backjumping does not affect solvability" $
      \test reorderGoals indepGoals ->
        let r1 = solve' (EnableBackjumping True) test
            r2 = solve' (EnableBackjumping False) test
            solve' enableBj =
              solve
                enableBj
                (FineGrainedConflicts False)
                reorderGoals
                (CountConflicts True)
                indepGoals
                (PreferOldest False)
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , testPropertyWithSeed "fine-grained conflicts does not affect solvability" $
      \test reorderGoals indepGoals ->
        let r1 = solve' (FineGrainedConflicts True) test
            r2 = solve' (FineGrainedConflicts False) test
            solve' fineGrainedConflicts =
              solve
                (EnableBackjumping True)
                fineGrainedConflicts
                reorderGoals
                (CountConflicts True)
                indepGoals
                (PreferOldest False)
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , testPropertyWithSeed "prefer oldest does not affect solvability" $
      \test reorderGoals indepGoals ->
        let r1 = solve' (PreferOldest True) test
            r2 = solve' (PreferOldest False) test
            solve' prefOldest =
              solve
                (EnableBackjumping True)
                (FineGrainedConflicts True)
                reorderGoals
                (CountConflicts True)
                indepGoals
                prefOldest
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                isRight (resultPlan r1) === isRight (resultPlan r2)
  , -- The next two tests use --no-count-conflicts, because the goal order used
    -- with --count-conflicts depends on the total set of conflicts seen by the
    -- solver. The solver explores more of the tree and encounters more
    -- conflicts when it doesn't backjump. The different goal orders can lead to
    -- different solutions and cause the test to fail.
    -- TODO: Find a faster way to randomly sort goals, and then use a random
    -- goal order in these tests.

    testPropertyWithSeed
      "backjumping does not affect the result (with static goal order)"
      $ \test reorderGoals indepGoals ->
        let r1 = solve' (EnableBackjumping True) test
            r2 = solve' (EnableBackjumping False) test
            solve' enableBj =
              solve
                enableBj
                (FineGrainedConflicts False)
                reorderGoals
                (CountConflicts False)
                indepGoals
                (PreferOldest False)
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                resultPlan r1 === resultPlan r2
  , testPropertyWithSeed
      "fine-grained conflicts does not affect the result (with static goal order)"
      $ \test reorderGoals indepGoals ->
        let r1 = solve' (FineGrainedConflicts True) test
            r2 = solve' (FineGrainedConflicts False) test
            solve' fineGrainedConflicts =
              solve
                (EnableBackjumping True)
                fineGrainedConflicts
                reorderGoals
                (CountConflicts False)
                indepGoals
                (PreferOldest False)
                Nothing
         in counterexample (showResults r1 r2) $
              noneReachedBackjumpLimit [r1, r2] ==>
                resultPlan r1 === resultPlan r2
  ]
  where
    noneReachedBackjumpLimit :: [Result] -> Bool
    noneReachedBackjumpLimit =
      not . any (\r -> resultPlan r == Left BackjumpLimitReached)

    showResults :: Result -> Result -> String
    showResults r1 r2 = showResult 1 r1 ++ showResult 2 r2

    showResult :: Int -> Result -> String
    showResult n result =
      unlines $
        ["", "Run " ++ show n ++ ":"]
          ++ resultLog result
          ++ ["result: " ++ show (resultPlan result)]

    implies :: Bool -> Bool -> Bool
    implies x y = not x || y

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

newtype VarOrdering = VarOrdering
  { unVarOrdering :: Variable P.QPN -> Variable P.QPN -> Ordering
  }

solve
  :: EnableBackjumping
  -> FineGrainedConflicts
  -> ReorderGoals
  -> CountConflicts
  -> IndependentGoals
  -> PreferOldest
  -> Maybe VarOrdering
  -> SolverTest
  -> Result
solve enableBj fineGrainedConflicts reorder countConflicts indep prefOldest goalOrder test =
  let (lg, result) =
        runProgress $
          exResolve
            (unTestDb (testDb test))
            Nothing
            Nothing
            (pkgConfigDbFromList [])
            (map unPN (testTargets test))
            -- The backjump limit prevents individual tests from using
            -- too much time and memory.
            (Just defaultMaxBackjumps)
            countConflicts
            fineGrainedConflicts
            (MinimizeConflictSet False)
            indep
            prefOldest
            reorder
            (AllowBootLibInstalls False)
            OnlyConstrainedNone
            enableBj
            (SolveExecutables True)
            (unVarOrdering <$> goalOrder)
            (testConstraints test)
            (testPreferences test)
            normal
            (EnableAllTests False)

      failure :: String -> Failure
      failure msg
        | "Backjump limit reached" `isInfixOf` msg = BackjumpLimitReached
        | otherwise = OtherFailure
   in Result
        { resultLog = lg
        , resultPlan =
            -- Force the result so that we check for internal errors when we check
            -- for success or failure. See D.C.Dependency.validateSolverResult.
            force $ either (Left . failure) (Right . extractInstallPlan) result
        }

-- | How to modify the order of the input targets.
data TargetOrder = SameOrder | ReverseOrder
  deriving (Show)

instance Arbitrary TargetOrder where
  arbitrary = elements [SameOrder, ReverseOrder]

  shrink SameOrder = []
  shrink ReverseOrder = [SameOrder]

data Result = Result
  { resultLog :: [String]
  , resultPlan :: Either Failure [(ExamplePkgName, ExamplePkgVersion)]
  }

data Failure = BackjumpLimitReached | OtherFailure
  deriving (Eq, Generic, Show)

instance NFData Failure

-- | Package name.
newtype PN = PN {unPN :: String}
  deriving (Eq, Ord, Show)

instance Arbitrary PN where
  arbitrary = PN <$> elements ("base" : [[pn] | pn <- ['A' .. 'G']])

-- | Package version.
newtype PV = PV {unPV :: Int}
  deriving (Eq, Ord, Show)

instance Arbitrary PV where
  arbitrary = PV <$> elements [1 .. 10]

type TestPackage = Either ExampleInstalled ExampleAvailable

getName :: TestPackage -> PN
getName = PN . either exInstName exAvName

getVersion :: TestPackage -> PV
getVersion = PV . either exInstVersion exAvVersion

data SolverTest = SolverTest
  { testDb :: TestDb
  , testTargets :: [PN]
  , testConstraints :: [ExConstraint]
  , testPreferences :: [ExPreference]
  }

-- | Pretty-print the test when quickcheck calls 'show'.
instance Show SolverTest where
  show test =
    let str =
          "SolverTest {testDb = "
            ++ show (testDb test)
            ++ ", testTargets = "
            ++ show (testTargets test)
            ++ ", testConstraints = "
            ++ show (testConstraints test)
            ++ ", testPreferences = "
            ++ show (testPreferences test)
            ++ "}"
     in maybe str valToStr $ parseValue str

instance Arbitrary SolverTest where
  arbitrary = do
    db <- arbitrary
    let pkgVersions = nub $ map (getName &&& getVersion) (unTestDb db)
        pkgs = nub $ map fst pkgVersions
    Positive n <- arbitrary
    targets <- randomSubset n pkgs
    constraints <- case pkgVersions of
      [] -> return []
      _ -> boundedListOf 1 $ arbitraryConstraint pkgVersions
    prefs <- case pkgVersions of
      [] -> return []
      _ -> boundedListOf 3 $ arbitraryPreference pkgVersions
    return (SolverTest db targets constraints prefs)

  shrink test =
    [test{testDb = db} | db <- shrink (testDb test)]
      ++ [test{testTargets = targets} | targets <- shrink (testTargets test)]
      ++ [test{testConstraints = cs} | cs <- shrink (testConstraints test)]
      ++ [test{testPreferences = prefs} | prefs <- shrink (testPreferences test)]

-- | Collection of source and installed packages.
newtype TestDb = TestDb {unTestDb :: ExampleDb}
  deriving (Show)

instance Arbitrary TestDb where
  arbitrary = do
    -- Avoid cyclic dependencies by grouping packages by name and only
    -- allowing each package to depend on packages in the groups before it.
    groupedPkgs <-
      shuffle . groupBy ((==) `on` fst) . nub . sort
        =<< boundedListOf 10 arbitrary
    db <- foldM nextPkgs (TestDb []) groupedPkgs
    TestDb <$> shuffle (unTestDb db)
    where
      nextPkgs :: TestDb -> [(PN, PV)] -> Gen TestDb
      nextPkgs db pkgs = TestDb . (++ unTestDb db) <$> traverse (nextPkg db) pkgs

      nextPkg :: TestDb -> (PN, PV) -> Gen TestPackage
      nextPkg db (pn, v) = do
        installed <- arbitrary
        if installed
          then Left <$> arbitraryExInst pn v (lefts $ unTestDb db)
          else Right <$> arbitraryExAv pn v db

  shrink (TestDb pkgs) = map TestDb $ shrink pkgs

arbitraryExAv :: PN -> PV -> TestDb -> Gen ExampleAvailable
arbitraryExAv pn v db =
  (\cds -> ExAv (unPN pn) (unPV v) cds []) <$> arbitraryComponentDeps pn db

arbitraryExInst :: PN -> PV -> [ExampleInstalled] -> Gen ExampleInstalled
arbitraryExInst pn v pkgs = do
  pkgHash <- vectorOf 10 $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
  numDeps <- min 3 <$> arbitrary
  deps <- randomSubset numDeps pkgs
  return $ ExInst (unPN pn) (unPV v) pkgHash (map exInstHash deps)

arbitraryComponentDeps :: PN -> TestDb -> Gen (ComponentDeps Dependencies)
arbitraryComponentDeps _ (TestDb []) = return $ CD.fromLibraryDeps (dependencies [])
arbitraryComponentDeps pn db = do
  -- dedupComponentNames removes components with duplicate names, for example,
  -- 'ComponentExe x' and 'ComponentTest x', and then CD.fromList combines
  -- duplicate unnamed components.
  cds <-
    CD.fromList . dedupComponentNames . filter (isValid . fst)
      <$> boundedListOf 5 (arbitraryComponentDep db)
  return $
    if isCompleteComponentDeps cds
      then cds
      else -- Add a library if the ComponentDeps isn't complete.
        CD.fromLibraryDeps (dependencies []) <> cds
  where
    isValid :: Component -> Bool
    isValid (ComponentSubLib name) = name /= mkUnqualComponentName (unPN pn)
    isValid _ = True

    dedupComponentNames =
      nubBy ((\x y -> isJust x && isJust y && x == y) `on` componentName . fst)

    componentName :: Component -> Maybe UnqualComponentName
    componentName ComponentLib = Nothing
    componentName ComponentSetup = Nothing
    componentName (ComponentSubLib n) = Just n
    componentName (ComponentFLib n) = Just n
    componentName (ComponentExe n) = Just n
    componentName (ComponentTest n) = Just n
    componentName (ComponentBench n) = Just n

-- | Returns true if the ComponentDeps forms a complete package, i.e., it
-- contains a library, exe, test, or benchmark.
isCompleteComponentDeps :: ComponentDeps a -> Bool
isCompleteComponentDeps = any (completesPkg . fst) . CD.toList
  where
    completesPkg ComponentLib = True
    completesPkg (ComponentExe _) = True
    completesPkg (ComponentTest _) = True
    completesPkg (ComponentBench _) = True
    completesPkg (ComponentSubLib _) = False
    completesPkg (ComponentFLib _) = False
    completesPkg ComponentSetup = False

arbitraryComponentDep :: TestDb -> Gen (ComponentDep Dependencies)
arbitraryComponentDep db = do
  comp <- arbitrary
  deps <- case comp of
    ComponentSetup -> smallListOf (arbitraryExDep db SetupDep)
    _ -> boundedListOf 5 (arbitraryExDep db NonSetupDep)
  return
    ( comp
    , Dependencies
        { depsExampleDependencies = deps
        , -- TODO: Test different values for visibility and buildability.
          depsVisibility = LibraryVisibilityPublic
        , depsIsBuildable = True
        }
    )

-- | Location of an 'ExampleDependency'. It determines which values are valid.
data ExDepLocation = SetupDep | NonSetupDep

arbitraryExDep :: TestDb -> ExDepLocation -> Gen ExampleDependency
arbitraryExDep db@(TestDb pkgs) level =
  let flag =
        ExFlagged
          <$> arbitraryFlagName
          <*> arbitraryDeps db
          <*> arbitraryDeps db
      other =
        -- Package checks require dependencies on "base" to have bounds.
        let notBase = filter ((/= PN "base") . getName) pkgs
         in [ExAny . unPN <$> elements (map getName notBase) | not (null notBase)]
              ++ [
                   -- existing version
                   let fixed pkg = ExFix (unPN $ getName pkg) (unPV $ getVersion pkg)
                    in fixed <$> elements pkgs
                 , -- random version of an existing package
                   ExFix . unPN . getName <$> elements pkgs <*> (unPV <$> arbitrary)
                 ]
   in oneof $
        case level of
          NonSetupDep -> flag : other
          SetupDep -> other

arbitraryDeps :: TestDb -> Gen Dependencies
arbitraryDeps db =
  frequency
    [ (1, return unbuildableDependencies)
    , (20, dependencies <$> smallListOf (arbitraryExDep db NonSetupDep))
    ]

arbitraryFlagName :: Gen String
arbitraryFlagName = (: []) <$> elements ['A' .. 'E']

arbitraryConstraint :: [(PN, PV)] -> Gen ExConstraint
arbitraryConstraint pkgs = do
  (PN pn, v) <- elements pkgs
  let anyQualifier = ScopeAnyQualifier (mkPackageName pn)
  oneof
    [ ExVersionConstraint anyQualifier <$> arbitraryVersionRange v
    , ExStanzaConstraint anyQualifier <$> sublistOf [TestStanzas, BenchStanzas]
    ]

arbitraryPreference :: [(PN, PV)] -> Gen ExPreference
arbitraryPreference pkgs = do
  (PN pn, v) <- elements pkgs
  oneof
    [ ExStanzaPref pn <$> sublistOf [TestStanzas, BenchStanzas]
    , ExPkgPref pn <$> arbitraryVersionRange v
    ]

arbitraryVersionRange :: PV -> Gen VersionRange
arbitraryVersionRange (PV v) =
  let version = mkSimpleVersion v
   in elements
        [ thisVersion version
        , notThisVersion version
        , earlierVersion version
        , orLaterVersion version
        , noVersion
        ]

instance Arbitrary ReorderGoals where
  arbitrary = ReorderGoals <$> arbitrary

  shrink (ReorderGoals reorder) = [ReorderGoals False | reorder]

instance Arbitrary IndependentGoals where
  arbitrary = IndependentGoals <$> arbitrary

  shrink (IndependentGoals indep) = [IndependentGoals False | indep]

instance Arbitrary Component where
  arbitrary =
    oneof
      [ return ComponentLib
      , ComponentSubLib <$> arbitraryUQN
      , ComponentExe <$> arbitraryUQN
      , ComponentFLib <$> arbitraryUQN
      , ComponentTest <$> arbitraryUQN
      , ComponentBench <$> arbitraryUQN
      , return ComponentSetup
      ]

  shrink ComponentLib = []
  shrink _ = [ComponentLib]

-- The "component-" prefix prevents component names and build-depends
-- dependency names from overlapping.
-- TODO: Remove the prefix once the QuickCheck tests support dependencies on
-- internal libraries.
arbitraryUQN :: Gen UnqualComponentName
arbitraryUQN =
  mkUnqualComponentName <$> (\c -> "component-" ++ [c]) <$> elements "ABC"

instance Arbitrary ExampleInstalled where
  arbitrary = error "arbitrary not implemented: ExampleInstalled"

  shrink ei =
    [ ei{exInstBuildAgainst = deps}
    | deps <- shrinkList shrinkNothing (exInstBuildAgainst ei)
    ]

instance Arbitrary ExampleAvailable where
  arbitrary = error "arbitrary not implemented: ExampleAvailable"

  shrink ea = [ea{exAvDeps = deps} | deps <- shrink (exAvDeps ea)]

instance (Arbitrary a, Monoid a) => Arbitrary (ComponentDeps a) where
  arbitrary = error "arbitrary not implemented: ComponentDeps"

  shrink = filter isCompleteComponentDeps . map CD.fromList . shrink . CD.toList

instance Arbitrary ExampleDependency where
  arbitrary = error "arbitrary not implemented: ExampleDependency"

  shrink (ExAny _) = []
  shrink (ExFix "base" _) = [] -- preserve bounds on base
  shrink (ExFix pn _) = [ExAny pn]
  shrink (ExFlagged flag th el) =
    depsExampleDependencies th
      ++ depsExampleDependencies el
      ++ [ExFlagged flag th' el | th' <- shrink th]
      ++ [ExFlagged flag th el' | el' <- shrink el]
  shrink dep = error $ "Dependency not handled: " ++ show dep

instance Arbitrary Dependencies where
  arbitrary = error "arbitrary not implemented: Dependencies"

  shrink deps =
    [deps{depsVisibility = v} | v <- shrink $ depsVisibility deps]
      ++ [deps{depsIsBuildable = b} | b <- shrink $ depsIsBuildable deps]
      ++ [deps{depsExampleDependencies = ds} | ds <- shrink $ depsExampleDependencies deps]

instance Arbitrary ExConstraint where
  arbitrary = error "arbitrary not implemented: ExConstraint"

  shrink (ExStanzaConstraint scope stanzas) =
    [ExStanzaConstraint scope stanzas' | stanzas' <- shrink stanzas]
  shrink (ExVersionConstraint scope vr) =
    [ExVersionConstraint scope vr' | vr' <- shrink vr]
  shrink _ = []

instance Arbitrary ExPreference where
  arbitrary = error "arbitrary not implemented: ExPreference"

  shrink (ExStanzaPref pn stanzas) =
    [ExStanzaPref pn stanzas' | stanzas' <- shrink stanzas]
  shrink (ExPkgPref pn vr) = [ExPkgPref pn vr' | vr' <- shrink vr]

instance Arbitrary OptionalStanza where
  arbitrary = error "arbitrary not implemented: OptionalStanza"

  shrink BenchStanzas = [TestStanzas]
  shrink TestStanzas = []

-- Randomly sorts solver variables using 'hash'.
-- TODO: Sorting goals with this function is very slow.
instance Arbitrary VarOrdering where
  arbitrary = do
    f <- arbitrary :: Gen (Int -> Int)
    return $ VarOrdering (comparing (f . hash))

instance Hashable pn => Hashable (Variable pn)
instance Hashable a => Hashable (P.Qualified a)
instance Hashable P.PackagePath
instance Hashable P.Qualifier
instance Hashable P.Namespace
instance Hashable OptionalStanza
instance Hashable FlagName
instance Hashable PackageName
instance Hashable ShortText

deriving instance Generic (Variable pn)
deriving instance Generic (P.Qualified a)
deriving instance Generic P.PackagePath
deriving instance Generic P.Namespace
deriving instance Generic P.Qualifier

randomSubset :: Int -> [a] -> Gen [a]
randomSubset n xs = take n <$> shuffle xs

boundedListOf :: Int -> Gen a -> Gen [a]
boundedListOf n gen = take n <$> listOf gen

-- | Generates lists with average length less than 1.
smallListOf :: Gen a -> Gen [a]
smallListOf gen =
  frequency
    [ (fr, vectorOf n gen)
    | (fr, n) <- [(3, 0), (5, 1), (2, 2)]
    ]

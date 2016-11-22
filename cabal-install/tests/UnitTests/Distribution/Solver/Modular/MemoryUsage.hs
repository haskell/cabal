-- | Tests for detecting space leaks in the dependency solver.
module UnitTests.Distribution.Solver.Modular.MemoryUsage (tests) where

import Test.Tasty (TestTree)

import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils

tests :: [TestTree]
tests = [
      runTest $ basicTest "basic space leak test"
    , runTest $ flagsTest "package with many flags"
    ]

-- | This test solves for n packages that each have two versions. Backjumping
-- is disabled, so the solver must explore a search tree of size 2^n. It should
-- fail if memory usage is proportional to the size of the tree.
basicTest :: String -> SolverTest
basicTest name =
    disableBackjumping $ mkTest pkgs name ["target"] anySolverFailure
  where
    n :: Int
    n = 18

    pkgs :: ExampleDb
    pkgs = map Right $
           [ exAv "target" 1 [ExAny $ pkgName 1]]
        ++ [ exAv (pkgName i) v [ExAny $ pkgName (i + 1)]
           | i <- [1..n], v <- [1, 2]]

    pkgName :: Int -> ExamplePkgName
    pkgName x = "pkg-" ++ show x

-- | This test is similar to 'basicTest', except that it has one package with n
-- flags, flag-1 through flag-n. The solver assigns flags in order, so it
-- doesn't discover the unknown dependencies under flag-n until it has assigned
-- all of the flags. It has to explore the whole search tree.
flagsTest :: String -> SolverTest
flagsTest name =
    disableBackjumping $
    goalOrder orderedFlags $ mkTest pkgs name ["pkg"] anySolverFailure
  where
    n :: Int
    n = 16

    pkgs :: ExampleDb
    pkgs = [Right $ exAv "pkg" 1 $
                [exFlag (flagName n) [ExAny "unknown1"] [ExAny "unknown2"]]

                -- The remaining flags have no effect:
             ++ [exFlag (flagName i) [] [] | i <- [1..n - 1]]
           ]

    flagName :: Int -> ExampleFlagName
    flagName x = "flag-" ++ show x

    orderedFlags :: [ExampleVar]
    orderedFlags = [F None "pkg" (flagName i) | i <- [1..n]]

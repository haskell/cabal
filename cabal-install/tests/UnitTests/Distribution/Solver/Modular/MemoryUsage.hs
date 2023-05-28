-- | Tests for detecting space leaks in the dependency solver.
module UnitTests.Distribution.Solver.Modular.MemoryUsage (tests) where

import Test.Tasty (TestTree)

import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils

tests :: [TestTree]
tests =
  [ runTest $ basicTest "basic space leak test"
  , runTest $ flagsTest "package with many flags"
  , runTest $ issue2899 "issue #2899"
  , runTest $ duplicateDependencies "duplicate dependencies"
  , runTest $ duplicateFlaggedDependencies "duplicate flagged dependencies"
  ]

-- | This test solves for n packages that each have two versions. There is no
-- solution, because the nth package depends on another package that doesn't fit
-- its version constraint. Backjumping and fine grained conflicts are disabled,
-- so the solver must explore a search tree of size 2^n. It should fail if
-- memory usage is proportional to the size of the tree.
basicTest :: String -> SolverTest
basicTest name =
  disableBackjumping $
    disableFineGrainedConflicts $
      mkTest pkgs name ["target"] anySolverFailure
  where
    n :: Int
    n = 18

    pkgs :: ExampleDb
    pkgs =
      map Right $
        [exAv "target" 1 [ExAny $ pkgName 1]]
          ++ [ exAv (pkgName i) v [ExRange (pkgName $ i + 1) 2 4]
             | i <- [1 .. n]
             , v <- [2, 3]
             ]
          ++ [exAv (pkgName $ n + 1) 1 []]

    pkgName :: Int -> ExamplePkgName
    pkgName x = "pkg-" ++ show x

-- | This test is similar to 'basicTest', except that it has one package with n
-- flags, flag-1 through flag-n. The solver assigns flags in order, so it
-- doesn't discover the unknown dependencies under flag-n until it has assigned
-- all of the flags. It has to explore the whole search tree.
flagsTest :: String -> SolverTest
flagsTest name =
  disableBackjumping $
    disableFineGrainedConflicts $
      goalOrder orderedFlags $
        mkTest pkgs name ["pkg"] anySolverFailure
  where
    n :: Int
    n = 16

    pkgs :: ExampleDb
    pkgs =
      [ Right $
          exAv "pkg" 1 $
            [exFlagged (numberedFlag n) [ExAny "unknown1"] [ExAny "unknown2"]]
              -- The remaining flags have no effect:
              ++ [exFlagged (numberedFlag i) [] [] | i <- [1 .. n - 1]]
      ]

    orderedFlags :: [ExampleVar]
    orderedFlags = [F QualNone "pkg" (numberedFlag i) | i <- [1 .. n]]

-- | Test for a space leak caused by sharing of search trees under packages with
-- link choices (issue #2899).
--
-- The goal order is fixed so that the solver chooses setup-dep and then
-- target-setup.setup-dep at the top of the search tree. target-setup.setup-dep
-- has two choices: link to setup-dep, and don't link to setup-dep. setup-dep
-- has a long chain of dependencies (pkg-1 through pkg-n). However, pkg-n
-- depends on pkg-n+1, which doesn't exist, so there is no solution. Since each
-- dependency has two versions, the solver must try 2^n combinations when
-- backjumping and fine grained conflicts are disabled. These combinations
-- create large search trees under each of the two choices for
-- target-setup.setup-dep. Although the choice to not link is disallowed by the
-- Single Instance Restriction, the solver doesn't know that until it has
-- explored (and evaluated) the whole tree under the choice to link. If the two
-- trees are shared, memory usage spikes.
issue2899 :: String -> SolverTest
issue2899 name =
  disableBackjumping $
    disableFineGrainedConflicts $
      goalOrder goals $
        mkTest pkgs name ["target"] anySolverFailure
  where
    n :: Int
    n = 16

    pkgs :: ExampleDb
    pkgs =
      map Right $
        [ exAv "target" 1 [ExAny "setup-dep"] `withSetupDeps` [ExAny "setup-dep"]
        , exAv "setup-dep" 1 [ExAny $ pkgName 1]
        ]
          ++ [ exAv (pkgName i) v [ExAny $ pkgName (i + 1)]
             | i <- [1 .. n]
             , v <- [1, 2]
             ]

    pkgName :: Int -> ExamplePkgName
    pkgName x = "pkg-" ++ show x

    goals :: [ExampleVar]
    goals = [P QualNone "setup-dep", P (QualSetup "target") "setup-dep"]

-- | Test for an issue related to lifting dependencies out of conditionals when
-- converting a PackageDescription to the solver's internal representation.
--
-- Issue:
-- For each conditional and each package B, the solver combined each dependency
-- on B in the true branch with each dependency on B in the false branch. It
-- added the combined dependencies to the build-depends outside of the
-- conditional. Since dependencies could be lifted out of multiple levels of
-- conditionals, the number of new dependencies could grow exponentially in the
-- number of levels. For example, the following package generated 4 copies of B
-- under flag-2=False, 8 copies under flag-1=False, and 16 copies at the top
-- level:
--
-- if flag(flag-1)
--   build-depends: B, B
-- else
--   if flag(flag-2)
--     build-depends: B, B
--   else
--     if flag(flag-3)
--       build-depends: B, B
--     else
--       build-depends: B, B
--
-- This issue caused the quickcheck tests to start frequently running out of
-- memory after an optimization that pruned unreachable branches (See PR #4929).
-- Each problematic test case contained at least one build-depends field with
-- duplicate dependencies, which was then duplicated under multiple levels of
-- conditionals by the solver's "buildable: False" transformation, when
-- "buildable: False" was under multiple flags. Finally, the branch pruning
-- feature put all build-depends fields in consecutive levels of the condition
-- tree, causing the solver's representation of the package to follow the
-- pattern in the example above.
--
-- Now the solver avoids this issue by combining all dependencies on the same
-- package before lifting them out of conditionals.
--
-- This test case is an expanded version of the example above, with library and
-- build-tool dependencies.
duplicateDependencies :: String -> SolverTest
duplicateDependencies name =
  mkTest pkgs name ["A"] $ solverSuccess [("A", 1), ("B", 1)]
  where
    copies, depth :: Int
    copies = 50
    depth = 50

    pkgs :: ExampleDb
    pkgs =
      [ Right $ exAv "A" 1 (dependencyTree 1)
      , Right $ exAv "B" 1 [] `withExe` exExe "exe" []
      ]

    dependencyTree :: Int -> [ExampleDependency]
    dependencyTree n
      | n > depth = buildDepends
      | otherwise =
          [ exFlagged
              (numberedFlag n)
              buildDepends
              (dependencyTree (n + 1))
          ]
      where
        buildDepends =
          replicate copies (ExFix "B" 1)
            ++ replicate copies (ExBuildToolFix "B" "exe" 1)

-- | This test is similar to duplicateDependencies, except that every dependency
-- on B is replaced by a conditional that contains B in both branches. It tests
-- that the solver doesn't just combine dependencies within one build-depends or
-- build-tool-depends field; it also needs to combine dependencies after they
-- are lifted out of conditionals.
duplicateFlaggedDependencies :: String -> SolverTest
duplicateFlaggedDependencies name =
  mkTest pkgs name ["A"] $ solverSuccess [("A", 1), ("B", 1)]
  where
    copies, depth :: Int
    copies = 15
    depth = 15

    pkgs :: ExampleDb
    pkgs =
      [ Right $ exAv "A" 1 (dependencyTree 1)
      , Right $ exAv "B" 1 [] `withExe` exExe "exe" []
      ]

    dependencyTree :: Int -> [ExampleDependency]
    dependencyTree n
      | n > depth = flaggedDeps
      | otherwise =
          [ exFlagged
              (numberedFlag n)
              flaggedDeps
              (dependencyTree (n + 1))
          ]
      where
        flaggedDeps = zipWith ($) (replicate copies flaggedDep) [0 :: Int ..]
        flaggedDep m =
          exFlagged
            (numberedFlag n ++ "-" ++ show m)
            buildDepends
            buildDepends
        buildDepends = [ExFix "B" 1, ExBuildToolFix "B" "exe" 1]

numberedFlag :: Int -> ExampleFlagName
numberedFlag n = "flag-" ++ show n

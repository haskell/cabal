module UnitTests.Distribution.Client.IndexUtils where

import Distribution.Client.IndexUtils
import Distribution.Client.IndexUtils.ActiveRepos
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Package
import Distribution.Simple.Utils (toUTF8LBS)
import Distribution.Solver.Types.PackageIndex (OverrideOrMerge (..))
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Types.LibraryName
import Distribution.Version

import qualified Data.List as List
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ simpleVersionsParserTests
  , indexCombiningTests
  , overrideOrMergeTests
  , deprecationAwareStrategyTests
  , addIndexTests
  ]

-- ---------------------------------------------------------------------------
-- Preferred-versions parser tests
-- ---------------------------------------------------------------------------

simpleVersionsParserTests :: TestTree
simpleVersionsParserTests =
  testGroup
    "Simple preferred-versions Parser Tests"
    [ testCase "simple deprecation dependency" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary < 0.9.0.0 || > 0.9.0.0")
        prefs
          @?= [ Right
                  ( Dependency
                      (mkPackageName "binary")
                      ( unionVersionRanges
                          (earlierVersion $ mkVersion [0, 9, 0, 0])
                          (laterVersion $ mkVersion [0, 9, 0, 0])
                      )
                      (NES.singleton LMainLibName)
                  )
              ]
    , testCase "multiple deprecation dependency" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary < 0.9.0.0 || > 0.9.0.0\ncontainers == 0.6.4.1")
        prefs
          @?= [ Right
                  ( Dependency
                      (mkPackageName "binary")
                      ( unionVersionRanges
                          (earlierVersion $ mkVersion [0, 9, 0, 0])
                          (laterVersion $ mkVersion [0, 9, 0, 0])
                      )
                      (NES.singleton LMainLibName)
                  )
              , Right
                  ( Dependency
                      (mkPackageName "containers")
                      (thisVersion $ mkVersion [0, 6, 4, 1])
                      (NES.singleton LMainLibName)
                  )
              ]
    , testCase "unparsable dependency" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary 0.9.0.0 || > 0.9.0.0")
        prefs
          @?= [ Left binaryDepParseError
              ]
    , testCase "partial parse" $ do
        let prefs = parsePreferredVersionsWarnings (toUTF8LBS "binary 0.9.0.0 || > 0.9.0.0\ncontainers == 0.6.4.1")
        prefs
          @?= [ Left binaryDepParseError
              , Right
                  ( Dependency
                      (mkPackageName "containers")
                      (thisVersion $ mkVersion [0, 6, 4, 1])
                      (NES.singleton LMainLibName)
                  )
              ]
    ]
  where
    binaryDepParseError =
      PreferredVersionsParseError
        { preferredVersionsParsecError =
            mconcat
              [ "\"<eitherParsec>\" (line 1, column 8):\n"
              , "unexpected '0'\n"
              , "expecting space, white space, opening paren, operator or end of input"
              ]
        , preferredVersionsOriginalDependency = "binary 0.9.0.0 || > 0.9.0.0"
        }

-- ---------------------------------------------------------------------------
-- Index-combining tests
--
-- These test 'applyStrategy' (exported from IndexUtils), which is the
-- per-repository step used by getSourcePackagesAtIndexState:
--
--   applyStrategy acc (_, Skip)     = acc
--   applyStrategy acc (idx, Merge)  = PackageIndex.merge acc idx
--   applyStrategy acc (idx, Override) = PackageIndex.override acc idx
--   pkgs = foldl' (\acc (rd, s) -> applyStrategy acc (rdIndex rd, s)) mempty pkgss'
-- ---------------------------------------------------------------------------

indexCombiningTests :: TestTree
indexCombiningTests =
  testGroup
    "Index combining (CombineStrategy)"
    [ testCase "Skip: repo contributes nothing" $
        pkgs [(repoFoo1, CombineStrategySkip)]
          @?= []
    , testCase "Merge: single repo makes all its packages visible" $
        pkgs [(repoFoo1, CombineStrategyMerge)]
          @?= [foo1]
    , testCase "Override: single repo makes all its packages visible" $
        pkgs [(repoFoo1, CombineStrategyOverride)]
          @?= [foo1]
    , testCase "Merge+Merge: non-overlapping packages are both visible" $
        pkgs [(repoFoo1, CombineStrategyMerge), (repoBar1, CombineStrategyMerge)]
          @?= List.sort [foo1, bar1]
    , testCase "Merge+Merge: different versions of same package are both visible" $
        pkgs [(repoFoo1, CombineStrategyMerge), (repoFoo2, CombineStrategyMerge)]
          @?= List.sort [foo1, foo2]
    , testCase "Merge+Override: packages only in first repo remain visible" $
        pkgs [(repoFoo1, CombineStrategyMerge), (repoBar1, CombineStrategyOverride)]
          @?= List.sort [foo1, bar1]
    , testCase "Merge+Override: override repo replaces all versions of overlapping package" $
        -- repoFoo12 has foo-1.0 and foo-2.0; repoFoo2 has only foo-2.0.
        -- Override means repoFoo2 wins the entire 'foo' bucket.
        pkgs [(repoFoo12, CombineStrategyMerge), (repoFoo2, CombineStrategyOverride)]
          @?= [foo2]
    , testCase "Merge+Override: override does not affect packages absent from override repo" $
        pkgs [(repoFoo1bar1, CombineStrategyMerge), (repoFoo2, CombineStrategyOverride)]
          @?= List.sort [foo2, bar1]
    , testCase "Skip in middle: skipped repo is ignored" $
        pkgs
          [ (repoFoo1, CombineStrategyMerge)
          , (repoFoo2, CombineStrategySkip)
          , (repoBar1, CombineStrategyMerge)
          ]
          @?= List.sort [foo1, bar1]
    , testCase "Skip+Merge: later merge after skip still contributes" $
        pkgs [(repoFoo1, CombineStrategySkip), (repoFoo2, CombineStrategyMerge)]
          @?= [foo2]
    , testCase "Override+Override: last override wins the package bucket" $
        pkgs
          [ (repoFoo1, CombineStrategyMerge)
          , (repoFoo2, CombineStrategyOverride)
          , (repoFoo3, CombineStrategyOverride)
          ]
          @?= [foo3]
    , testCase "Override+Merge: merge after override combines both buckets" $
        -- foo bucket starts as {foo-2.0} after override, then merges {foo-3.0}
        -- giving {foo-2.0, foo-3.0}
        pkgs
          [ (repoFoo1, CombineStrategyMerge)
          , (repoFoo2, CombineStrategyOverride)
          , (repoFoo3, CombineStrategyMerge)
          ]
          @?= List.sort [foo2, foo3]
    , testCase "All skip: result is empty" $
        pkgs
          [ (repoFoo1, CombineStrategySkip)
          , (repoFoo2, CombineStrategySkip)
          ]
          @?= []
    , testCase "Empty repos list: result is empty" $
        pkgs [] @?= []
    ]

-- Run the combining fold and return the result as a sorted list of PackageIds.
-- Uses the exported 'applyStrategy' from IndexUtils directly, so this stays
-- in sync with the production implementation in getSourcePackagesAtIndexState.
pkgs
  :: [(PackageIndex.PackageIndex PackageIdentifier, CombineStrategy)]
  -> [PackageIdentifier]
pkgs = List.sort . PackageIndex.allPackages . List.foldl' applyStrategy mempty

-- Test packages
foo1, foo2, foo3, bar1 :: PackageIdentifier
foo1 = PackageIdentifier (mkPackageName "foo") (mkVersion [1, 0])
foo2 = PackageIdentifier (mkPackageName "foo") (mkVersion [2, 0])
foo3 = PackageIdentifier (mkPackageName "foo") (mkVersion [3, 0])
bar1 = PackageIdentifier (mkPackageName "bar") (mkVersion [1, 0])

-- Single-package indices
repoFoo1, repoFoo2, repoFoo3, repoBar1 :: PackageIndex.PackageIndex PackageIdentifier
repoFoo1 = PackageIndex.fromList [foo1]
repoFoo2 = PackageIndex.fromList [foo2]
repoFoo3 = PackageIndex.fromList [foo3]
repoBar1 = PackageIndex.fromList [bar1]

-- Multi-package indices
repoFoo12, repoFoo1bar1 :: PackageIndex.PackageIndex PackageIdentifier
repoFoo12 = PackageIndex.fromList [foo1, foo2]
repoFoo1bar1 = PackageIndex.fromList [foo1, bar1]

-- ---------------------------------------------------------------------------
-- overrideOrMerge tests
--
-- These test PackageIndex.overrideOrMerge directly, which is the building
-- block for conditionally falling back to merge when all versions of a
-- package in the override repo are deprecated (issue #8502).
-- ---------------------------------------------------------------------------

overrideOrMergeTests :: TestTree
overrideOrMergeTests =
  testGroup
    "overrideOrMerge"
    [ testCase "all-Override strategy matches plain override" $
        -- When strategy always returns Override, result equals PackageIndex.override
        let result = PackageIndex.overrideOrMerge (const Override) repoFoo12 repoFoo2
            expected = PackageIndex.override repoFoo12 repoFoo2
         in allPkgs result @?= allPkgs expected
    , testCase "all-Merge strategy matches plain merge" $
        -- When strategy always returns Merge, result equals PackageIndex.merge
        let result = PackageIndex.overrideOrMerge (const Merge) repoFoo12 repoFoo2
            expected = PackageIndex.merge repoFoo12 repoFoo2
         in allPkgs result @?= allPkgs expected
    , testCase "Override: second index wins entire package bucket" $
        -- repoFoo12 has foo-1.0 and foo-2.0; repoFoo2 has only foo-2.0.
        -- Override means repoFoo2 wins the 'foo' bucket, so foo-1.0 is hidden.
        allPkgs (PackageIndex.overrideOrMerge (const Override) repoFoo12 repoFoo2)
          @?= [foo2]
    , testCase "Merge: both buckets combined, duplicates removed" $
        -- repoFoo12 has foo-1.0 and foo-2.0; repoFoo2 has only foo-2.0.
        -- Merge keeps foo-1.0 and foo-2.0 (foo-2.0 deduplicated).
        allPkgs (PackageIndex.overrideOrMerge (const Merge) repoFoo12 repoFoo2)
          @?= List.sort [foo1, foo2]
    , testCase "mixed strategy: Override for foo, Merge for bar" $
        -- repoFoo1bar1 has foo-1.0 and bar-1.0; second index has foo-2.0 and bar-1.0.
        -- foo is overridden (foo-1.0 hidden), bar is merged (bar-1.0 deduplicated).
        let i2 = PackageIndex.fromList [foo2, bar1]
            strategy name
              | name == mkPackageName "foo" = Override
              | otherwise = Merge
         in allPkgs (PackageIndex.overrideOrMerge strategy repoFoo1bar1 i2)
              @?= List.sort [foo2, bar1]
    , testCase "deprecated fallback: Merge when override repo has only deprecated versions" $
        -- Simulates the issue-8502 scenario: repo-a has foo-1.0; repo-b (override)
        -- has foo-2.0 but all its versions are deprecated.  The caller detects this
        -- and passes Merge for 'foo', so foo-1.0 remains visible alongside foo-2.0.
        let repoA = PackageIndex.fromList [foo1]
            repoB = PackageIndex.fromList [foo2] -- pretend all deprecated
            strategy name
              | name == mkPackageName "foo" = Merge -- fall back because all deprecated
              | otherwise = Override
         in allPkgs (PackageIndex.overrideOrMerge strategy repoA repoB)
              @?= List.sort [foo1, foo2]
    , testCase "package absent from second index: first index versions kept" $
        -- bar is only in repoFoo1bar1, not in repoFoo2; it survives regardless of strategy.
        allPkgs (PackageIndex.overrideOrMerge (const Override) repoFoo1bar1 repoFoo2)
          @?= List.sort [foo2, bar1]
    , testCase "package absent from first index: second index versions appear" $
        allPkgs (PackageIndex.overrideOrMerge (const Override) repoFoo1 repoBar1)
          @?= List.sort [foo1, bar1]
    , testCase "empty first index: second index fully visible" $
        allPkgs (PackageIndex.overrideOrMerge (const Override) mempty repoFoo12)
          @?= List.sort [foo1, foo2]
    , testCase "empty second index: first index unchanged" $
        allPkgs (PackageIndex.overrideOrMerge (const Override) repoFoo12 mempty)
          @?= List.sort [foo1, foo2]
    ]

allPkgs :: PackageIndex.PackageIndex PackageIdentifier -> [PackageIdentifier]
allPkgs = List.sort . PackageIndex.allPackages

-- ---------------------------------------------------------------------------
-- deprecationAwareStrategy tests
--
-- Tests for the per-package Override/Merge decision used when applying a
-- CombineStrategyOverride repo.  The three cases are:
--   1. Package absent from preferred-versions  -> Override
--   2. Package present, some versions preferred -> Override
--   3. Package present, no versions preferred  -> Merge (all deprecated)
-- ---------------------------------------------------------------------------

deprecationAwareStrategyTests :: TestTree
deprecationAwareStrategyTests =
  testGroup
    "deprecationAwareStrategy"
    [ testCase "package absent from preferred-versions gives Override" $
        -- No entry for 'foo' in prefs, so the repo is not restricting it.
        strat repoFoo1 Map.empty fooName @?= Override
    , testCase "package present with matching versions gives Override" $
        -- foo-1.0 is in the index and satisfies ">= 1.0", so not all deprecated.
        strat repoFoo1 (prefs fooName (orLaterVersion v1)) fooName @?= Override
    , testCase "package present but no versions match gives Merge" $
        -- foo-1.0 is in the index but the pref ">= 2.0" excludes it: all deprecated.
        strat repoFoo1 (prefs fooName (orLaterVersion v2)) fooName @?= Merge
    , testCase "unrelated package in prefs does not affect result" $
        -- Prefs only mention 'bar'; 'foo' has no pref entry, so Override.
        strat repoFoo1 (prefs barName (orLaterVersion v1)) fooName @?= Override
    , testCase "package absent from index but in prefs gives Merge" $
        -- The pref entry exists but the index is empty, so lookupDependency
        -- returns [], meaning no preferred version exists.
        strat mempty (prefs fooName (orLaterVersion v2)) fooName @?= Merge
    , testCase "multiple packages decided independently" $
        -- foo is deprecated (pref excludes foo-1.0), bar is not (bar-1.0 satisfies >= 1.0).
        let p =
              Map.unionWith
                intersectVersionRanges
                (prefs fooName (orLaterVersion v2))
                (prefs barName (orLaterVersion v1))
         in do
              strat repoFoo1bar1 p fooName @?= Merge
              strat repoFoo1bar1 p barName @?= Override
    ]
  where
    strat = deprecationAwareStrategy
    fooName = mkPackageName "foo"
    barName = mkPackageName "bar"
    v1 = mkVersion [1, 0]
    v2 = mkVersion [2, 0]
    prefs name vr = Map.singleton name vr

-- ---------------------------------------------------------------------------
-- addIndex tests
--
-- Tests for the top-level addIndex, which is the function used by
-- getSourcePackagesAtIndexState to fold each repository's index into the
-- accumulator.  Unlike applyStrategy, addIndex consults preferred-versions
-- for CombineStrategyOverride, falling back to merge when all versions of a
-- package are deprecated.  A regression to plain override would cause the
-- "all deprecated" test to fail.
-- ---------------------------------------------------------------------------

addIndexTests :: TestTree
addIndexTests =
  testGroup
    "addIndex"
    [ testCase "Skip: index not added" $
        run [(repoFoo1, [], CombineStrategySkip)]
          @?= []
    , testCase "Merge: index added" $
        run [(repoFoo1, [], CombineStrategyMerge)]
          @?= [foo1]
    , testCase "Override with no prefs: behaves like plain override" $
        run
          [ (repoFoo12, [], CombineStrategyMerge)
          , (repoFoo2, [], CombineStrategyOverride)
          ]
          @?= [foo2]
    , testCase "Override with prefs matching some versions: still overrides" $
        -- foo-2.0 satisfies ">= 2.0", so not all deprecated; override applies.
        run
          [ (repoFoo12, [], CombineStrategyMerge)
          , (repoFoo2, [dep fooName (orLaterVersion v2)], CombineStrategyOverride)
          ]
          @?= [foo2]
    , testCase "Override with all versions deprecated: falls back to merge" $
        -- foo-2.0 does not satisfy ">= 3.0", so all versions deprecated;
        -- override falls back to merge, keeping foo-1.0 from the first repo.
        run
          [ (repoFoo1, [], CombineStrategyMerge)
          , (repoFoo2, [dep fooName (orLaterVersion v3)], CombineStrategyOverride)
          ]
          @?= List.sort [foo1, foo2]
    , testCase "Override: only the deprecated package falls back, others still override" $
        -- foo is all-deprecated in override repo → merge; bar has no prefs → override.
        run
          [ (repoFoo1bar1, [], CombineStrategyMerge)
          ,
            ( PackageIndex.fromList [foo2, bar1]
            , [dep fooName (orLaterVersion v3)]
            , CombineStrategyOverride
            )
          ]
          @?= List.sort [foo1, foo2, bar1]
    ]
  where
    run = allPkgs . List.foldl' (\acc (idx, ps, s) -> addIndex acc (idx, ps, s)) mempty
    fooName = mkPackageName "foo"
    v2 = mkVersion [2, 0]
    v3 = mkVersion [3, 0]
    dep name vr = Dependency name vr (NES.singleton LMainLibName)

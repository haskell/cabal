module UnitTests.Distribution.Client.IndexUtils where

import Distribution.Client.IndexUtils
import Distribution.Client.IndexUtils.ActiveRepos
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Package
import Distribution.Simple.Utils (toUTF8LBS)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Types.LibraryName
import Distribution.Version

import qualified Data.List as List

import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ simpleVersionsParserTests
  , indexCombiningTests
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

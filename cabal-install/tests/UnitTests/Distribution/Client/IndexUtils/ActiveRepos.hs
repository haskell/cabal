module UnitTests.Distribution.Client.IndexUtils.ActiveRepos (tests) where

import Distribution.Client.IndexUtils.ActiveRepos
import Distribution.Client.Types.RepoName (RepoName (..))
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)

import UnitTests.Distribution.Client.ArbitraryInstances ()

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testGroup "organizeByRepos" organizeByReposTests
  , testGroup "filterSkippedActiveRepos" filterSkippedTests
  , testGroup
      "parse/pretty roundtrip"
      [ testProperty "ActiveRepos roundtrips" prop_activeReposRoundtrip
      ]
  ]

-------------------------------------------------------------------------------
-- organizeByRepos
-------------------------------------------------------------------------------

-- Convenience: run organizeByRepos over a fixed three-element repo list.
organize :: ActiveRepos -> Either String [(RepoName, CombineStrategy)]
organize ar = organizeByRepos ar id [RepoName "a", RepoName "b", RepoName "c"]

organizeByReposTests :: [TestTree]
organizeByReposTests =
  [ testCase ":rest assigns strategy to all repos in order" $
      organize (ActiveRepos [ActiveRepoRest CombineStrategyMerge])
        @?= Right
          [ (RepoName "a", CombineStrategyMerge)
          , (RepoName "b", CombineStrategyMerge)
          , (RepoName "c", CombineStrategyMerge)
          ]
  , testCase ":none yields empty result" $
      organize (ActiveRepos [])
        @?= Right []
  , testCase "named repo before :rest is placed first" $
      organize
        ( ActiveRepos
            [ ActiveRepo (RepoName "b") CombineStrategyOverride
            , ActiveRepoRest CombineStrategyMerge
            ]
        )
        @?= Right
          [ (RepoName "b", CombineStrategyOverride)
          , (RepoName "a", CombineStrategyMerge)
          , (RepoName "c", CombineStrategyMerge)
          ]
  , testCase "named repo after :rest is placed last" $
      organize
        ( ActiveRepos
            [ ActiveRepoRest CombineStrategyMerge
            , ActiveRepo (RepoName "b") CombineStrategyOverride
            ]
        )
        @?= Right
          [ (RepoName "a", CombineStrategyMerge)
          , (RepoName "c", CombineStrategyMerge)
          , (RepoName "b", CombineStrategyOverride)
          ]
  , testCase "named repo absent from provided list gives Left" $
      organize
        ( ActiveRepos
            [ ActiveRepoRest CombineStrategyMerge
            , ActiveRepo (RepoName "d") CombineStrategyOverride
            ]
        )
        @?= Left "no repository provided d"
  , testCase "named repo against empty list gives Left" $
      organizeByRepos
        (ActiveRepos [ActiveRepo (RepoName "a") CombineStrategyMerge])
        id
        ([] :: [RepoName])
        @?= Left "no repository provided a"
  , testCase "skip strategy is preserved in output" $
      organize
        ( ActiveRepos
            [ ActiveRepo (RepoName "a") CombineStrategySkip
            , ActiveRepoRest CombineStrategyMerge
            ]
        )
        @?= Right
          [ (RepoName "a", CombineStrategySkip)
          , (RepoName "b", CombineStrategyMerge)
          , (RepoName "c", CombineStrategyMerge)
          ]
  , testCase ":rest with skip strategy skips all remaining repos" $
      organize (ActiveRepos [ActiveRepoRest CombineStrategySkip])
        @?= Right
          [ (RepoName "a", CombineStrategySkip)
          , (RepoName "b", CombineStrategySkip)
          , (RepoName "c", CombineStrategySkip)
          ]
  , testCase "multiple :rest entries cause each repo to appear once per :rest" $
      -- Documented edge case: if ActiveRepoRest appears more than once,
      -- the rest-repositories appear multiple times in the output.
      organize
        ( ActiveRepos
            [ ActiveRepoRest CombineStrategyMerge
            , ActiveRepoRest CombineStrategyOverride
            ]
        )
        @?= Right
          [ (RepoName "a", CombineStrategyMerge)
          , (RepoName "b", CombineStrategyMerge)
          , (RepoName "c", CombineStrategyMerge)
          , (RepoName "a", CombineStrategyOverride)
          , (RepoName "b", CombineStrategyOverride)
          , (RepoName "c", CombineStrategyOverride)
          ]
  ]

-------------------------------------------------------------------------------
-- filterSkippedActiveRepos
-------------------------------------------------------------------------------

filterSkippedTests :: [TestTree]
filterSkippedTests =
  [ testCase "skipped entries are removed when no :rest is present" $
      filterSkippedActiveRepos
        ( ActiveRepos
            [ ActiveRepo (RepoName "a") CombineStrategyMerge
            , ActiveRepo (RepoName "b") CombineStrategySkip
            ]
        )
        @?= ActiveRepos [ActiveRepo (RepoName "a") CombineStrategyMerge]
  , testCase "all-skipped list with no :rest yields empty" $
      filterSkippedActiveRepos
        ( ActiveRepos
            [ ActiveRepo (RepoName "a") CombineStrategySkip
            , ActiveRepo (RepoName "b") CombineStrategySkip
            ]
        )
        @?= ActiveRepos []
  , testCase "list without any skipped entries is unchanged" $
      let ar =
            ActiveRepos
              [ ActiveRepo (RepoName "a") CombineStrategyMerge
              , ActiveRepo (RepoName "b") CombineStrategyOverride
              ]
       in filterSkippedActiveRepos ar @?= ar
  , testCase "skipped entries are kept when :rest is present" $
      -- filterSkippedActiveRepos is a no-op when ActiveRepoRest appears
      let ar =
            ActiveRepos
              [ ActiveRepoRest CombineStrategyMerge
              , ActiveRepo (RepoName "b") CombineStrategySkip
              ]
       in filterSkippedActiveRepos ar @?= ar
  , testCase ":rest with skip strategy is kept unchanged" $
      let ar = ActiveRepos [ActiveRepoRest CombineStrategySkip]
       in filterSkippedActiveRepos ar @?= ar
  ]

-------------------------------------------------------------------------------
-- Parse/pretty roundtrip
-------------------------------------------------------------------------------

prop_activeReposRoundtrip :: ActiveRepos -> Property
prop_activeReposRoundtrip ar =
  counterexample ("prettyShow: " ++ prettyShow ar) $
    simpleParsec (prettyShow ar) === Just ar

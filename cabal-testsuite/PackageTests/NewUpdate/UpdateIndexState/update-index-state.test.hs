import Test.Cabal.Prelude

main = cabalTest $ withRemoteRepo "repo" $ do
  -- The _first_ update call causes a warning about missing mirrors, the warning
  -- is platform-dependent and it's not part of the test expectations, so we
  -- check the output manually.
  res <- recordMode DoNotRecord $
           cabal' "update" ["repository.localhost,2022-01-28T02:36:41Z"]
  assertOutputContains "The index-state is set to 2022-01-28T02:36:41Z" res
  assertOutputDoesNotContain "revert" res
  cabal "update" ["repository.localhost,2016-09-24T17:47:48Z"]
  cabal "update" ["repository.localhost,2022-01-28T02:36:41Z"]

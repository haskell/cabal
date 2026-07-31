import Test.Cabal.Prelude
main = do
  cabalTest . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run"]
    assertOutputContains "not a user-provided goal" res

  -- Not really a version constrained dependency.
  cabalTest' "any-flag" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib -any"]
    assertOutputContains "not a user-provided goal" res

  -- Not really a version constrained dependency.
  cabalTest' "any-version" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib >=0"]
    assertOutputContains "not a user-provided goal" res

  -- TODO: Find out why isn't -none the same as <0?
  cabalTest' "none-flag" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib -none"]
    assertOutputDoesNotContain "not a user-provided goal" res

  -- Doesn't get as far as the constraint check because the <0 version
  -- constraint is unsatisfiable and the dependency is rejected.
  cabalTest' "none-version" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib <0"]
    assertOutputContains " rejecting: some-lib-1.0 (constraint from command line flag requires <0)" res
    assertOutputDoesNotContain "not a user-provided goal" res

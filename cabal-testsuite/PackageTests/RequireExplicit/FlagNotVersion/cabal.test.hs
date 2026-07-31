import Test.Cabal.Prelude
main = do
  cabalTest . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run"]
    assertOutputContains "not a user-provided goal" res

  -- The following all check version ranges that don't have a version
  -- constrained dependency one way or another.

  cabalTest' "any-flag" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib -any"]
    assertOutputContains "not a user-provided goal" res

  cabalTest' "any-version" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib >=0"]
    assertOutputContains "not a user-provided goal" res

  cabalTest' "none-flag" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib -none"]
    assertOutputContains "not a user-provided goal" res

  cabalTest' "none-version" . recordMode RecordMarked $ withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--constraint", "some-lib <0"]
    assertOutputContains "not a user-provided goal" res

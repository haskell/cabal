import Test.Cabal.Prelude
main = cabalTest . recordMode RecordMarked $ withRepo "repo" $ do
  res <- fails $ cabal' "v2-build" ["all", "--dry-run"]
  assertOutputContains "not a user-provided goal" res

import Test.Cabal.Prelude
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $ withRepo "repo" $ do
  -- other-lib is a dependency, but it's not listed in cabal.project
  res <- fails $ cabal' "v2-build" ["all", "--dry-run"]
  assertOutputContains "not a user-provided goal" res

import Test.Cabal.Prelude
-- See #4332, dep solving output is not deterministic
main = cabalTest . recordMode DoNotRecord $ withRepo "repo" $ do
  -- other-lib is a dependency of b, but it's not listed in cabal.project
  res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--reject-unconstrained-dependencies", "all", "--constraint", "some-exe -any"]
  assertOutputContains "not a user-provided goal" res

  -- and some-exe is a build-tool dependency of b, again not listed
  res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--reject-unconstrained-dependencies", "all", "--constraint", "other-lib -any"]
  assertOutputContains "not a user-provided goal" res

  -- everything's listed, but -any is not a version constraint accepted by reject-unconstrained-dependencies=all
  res <- fails $ cabal' "v2-build" ["all", "--dry-run", "--reject-unconstrained-dependencies", "all", "--constraint", "other-lib -any", "--constraint", "some-exe -any"]
  assertOutputContains "not a user-provided goal" res

  -- everything's listed, good to go if we give proper version constraints
  res <- cabal' "v2-build" ["all", "--dry-run", "--reject-unconstrained-dependencies", "all", "--constraint", "some-lib ==1.0", "--constraint", "other-lib ==1.0", "--constraint", "some-exe ==1.0"]
  assertOutputDoesNotContain "not a user-provided goal" res

  -- a depends on b, but b is a local dependency, so it gets a pass, but we still need to provide version constraints for the other dependencies
  res <- fails $ cabal' "v2-build" ["a", "--dry-run", "--reject-unconstrained-dependencies", "all", "--constraint", "other-lib -any", "--constraint", "some-exe -any"]
  assertOutputContains "not a user-provided goal" res

  -- a depends on b, but b is a local dependency, so it gets a pass and we're good to go if we give proper version constraints
  res <- cabal' "v2-build" ["a", "--dry-run", "--reject-unconstrained-dependencies", "all", "--constraint", "some-lib ==1.0", "--constraint", "other-lib ==1.0", "--constraint", "some-exe ==1.0"]
  assertOutputDoesNotContain "not a user-provided goal" res

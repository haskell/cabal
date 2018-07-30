import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  res <- cabal' "outdated" ["--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res

  -- Test last-one-wins behaviour.
  res <- cabal' "outdated" ["--project-file", "cabal.project", "--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res

  res <- cabal' "outdated" ["--new-freeze-file", "--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res

  res <- cabal' "outdated" ["--project-file", "variant.project", "--new-freeze-file"]
  assertOutputContains "base" res
  assertOutputContains "template-haskell" res

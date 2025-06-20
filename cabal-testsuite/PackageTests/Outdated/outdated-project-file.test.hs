import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  res <- cabal' "outdated" ["--v2-freeze-file", "--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res
  assertOutputDoesNotContain "binary" res

  -- Test last-one-wins behaviour.
  res <- cabal' "outdated" ["--v2-freeze-file", "--project-file", "cabal.project", "--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res
  assertOutputDoesNotContain "binary" res

  cabal "outdated" ["--project-file", "variant.project"]

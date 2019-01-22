import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  res <- cabal' "outdated" ["--v2-freeze-file", "--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res

  -- Test last-one-wins behaviour.
  res <- cabal' "outdated" ["--v2-freeze-file", "--project-file", "cabal.project", "--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res

  -- Test for erroring on --project-file without --v2-freeze-file
  fails $ cabal "outdated" ["--project-file", "variant.project"]

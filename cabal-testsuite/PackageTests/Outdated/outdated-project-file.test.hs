import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  res <- cabal' "outdated" ["--project-file", "variant.project"]
  assertOutputContains "base" res
  assertOutputDoesNotContain "template-haskell" res


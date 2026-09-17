import Test.Cabal.Prelude

main = cabalTest . recordMode DoNotRecord $ do
  withRepo "repo" $ do
    res <- fails $ cabal' "v2-build" ["fake-pkg"]
    assertOutputContains "unknown package: p" res
    assertOutputContains "searched repositories: test-local-repo" res

import Test.Cabal.Prelude

main = cabalTest $ do
  -- This is the head index-state
  iso8601ParseM "2023-01-01T00:00:00Z"
    >>= setModificationTime ("repo" </> "pkg-1.0/pkg.cabal")

  withSecureRepo "repo" $ do
    cabal "update" []

    -- Check that we mention the previous timestamp
    res <- cabal' "update" ["test-local-repo,2022-01-01T00:00:00Z"]
    assertOutputContains "test-local-repo,2023-01-01T00:00:00Z" res

    cabal "update" ["test-local-repo,2022-01-01T00:00:00Z"]

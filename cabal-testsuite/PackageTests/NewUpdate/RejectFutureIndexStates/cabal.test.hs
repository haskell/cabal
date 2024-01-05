import Test.Cabal.Prelude

main =
  cabalTest $
    withProjectFile "cabal.project" $ do
      -- This is the head index-state
      iso8601ParseM "2023-12-25T00:00:00Z"
        >>= setModificationTime "repo/pkg-1.0/pkg.cabal"
      withSecureRepo "repo" $ do
        cabal "update" []
        -- This shall fail with an error message as specified in `cabal.out`
        fails $ cabal "build" ["--index-state=4000-01-01T00:00:00Z", "fake-pkg"]
        -- This shall fail by not finding the package, what indicates that it
        -- accepted an older index-state.
        fails $ cabal "build" ["--index-state=2023-01-01T00:00:00Z", "fake-pkg"]

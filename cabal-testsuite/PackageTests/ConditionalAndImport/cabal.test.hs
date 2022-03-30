import Test.Cabal.Prelude
main = cabalTest $
  withRepo "repo" $ do
     cabal "v2-run" [ "some-exe" ]
     fails $ cabal "v2-build" [ "--project=cabal-cyclical.project" ]
     fails $ cabal "v2-build" [ "--project=cabal-bad-conditional.project" ]

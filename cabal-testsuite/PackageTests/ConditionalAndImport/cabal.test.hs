import Test.Cabal.Prelude
main = cabalTest $
  withRepo "repo" $ do
     cabal "v2-run" [ "some-exe" ]
     fails $ cabal "v2-build" [ "--project-file=cabal-cyclical.project" ]
     fails $ cabal "v2-build" [ "--project-file=cabal-bad-conditional.project" ]

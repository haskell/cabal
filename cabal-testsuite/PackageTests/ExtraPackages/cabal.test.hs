import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  cabal "v2-run" [ "some-exe" ]

import Test.Cabal.Prelude
main =
  cabalTest $ withShorterPathForNewBuildStore $ do
    skipUnlessGhcVersion ">= 8.1"
    withRepo "repo" $ do
      cabal "v2-build" ["T6385"]

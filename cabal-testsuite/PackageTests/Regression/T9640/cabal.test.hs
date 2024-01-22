import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  skipUnlessGhcVersion ">= 8.8"
  cabal "build" ["depend-on-custom-with-exe"]

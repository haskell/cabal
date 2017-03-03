import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "new-build" ["build-tools-path-internal", "hello-world"]

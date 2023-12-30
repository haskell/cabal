import Test.Cabal.Prelude
-- Test leacy `build-tools` dependency on sublibrary
main = cabalTest $ do
    cabal "v2-build" ["foo", "hello-world"]

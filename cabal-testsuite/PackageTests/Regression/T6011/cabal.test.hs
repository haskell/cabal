import Test.Cabal.Prelude
-- Test build-tool-depends between two packages
main = cabalTest $ do
    cabal "v2-build" ["client", "--disable-per-component"]

import Test.Cabal.Prelude
main = cabalTest $ do
  cabal "v2-sdist" ["a", "--list-only"]


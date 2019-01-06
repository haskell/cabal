import Test.Cabal.Prelude
main = cabalTest $ withSourceCopy $ do
  cabal "v2-sdist" ["a", "--list-only"]


import Test.Cabal.Prelude
main = cabalTest $ withSourceCopy $ do
  cabal "new-sdist" ["a", "--list-only"]


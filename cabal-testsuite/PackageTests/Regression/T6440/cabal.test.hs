import Test.Cabal.Prelude
main = cabalTest $ cabal "test" ["--enable-coverage"]

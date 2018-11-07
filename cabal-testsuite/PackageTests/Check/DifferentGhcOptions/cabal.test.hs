import Test.Cabal.Prelude
main = cabalTest $ fails $ cabal "check" []

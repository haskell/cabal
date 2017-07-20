import Test.Cabal.Prelude
main = cabalTest $
    cabal' "new-run" ["foo"] >>= assertOutputContains "Hello World"


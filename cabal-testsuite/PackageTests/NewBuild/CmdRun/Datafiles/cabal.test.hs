import Test.Cabal.Prelude
main = cabalTest $ do
    cabal' "v2-run" ["foo"] >>= assertOutputContains "Hello World"
    cabal' "v2-run" ["bar"] >>= assertOutputContains "Hello World"


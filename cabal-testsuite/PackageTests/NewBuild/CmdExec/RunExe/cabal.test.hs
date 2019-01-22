import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-build" []
    cabal' "v2-exec" ["foo"] >>= assertOutputContains "Hello World"


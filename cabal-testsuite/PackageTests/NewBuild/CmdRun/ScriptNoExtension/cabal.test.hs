import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-run" ["with sp"] >>= assertOutputContains "Hello World"

import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-run" ["with space"] >>= assertOutputContains "Hello World"

import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-run" ["script with spaces"] >>= assertOutputContains "Hello World"

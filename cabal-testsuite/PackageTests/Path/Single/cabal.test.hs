import Test.Cabal.Prelude

main = cabalTest . void $ cabal "path" ["-z", "--output-format=key-value", "--installdir"]

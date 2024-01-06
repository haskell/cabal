import Test.Cabal.Prelude

main = cabalTest . void $ cabal "path" ["--installdir"]

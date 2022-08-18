import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabalWithStdin "v2-repl" ["script.hs"] ""
    cabalWithStdin "v2-repl" ["script.hs"] ""

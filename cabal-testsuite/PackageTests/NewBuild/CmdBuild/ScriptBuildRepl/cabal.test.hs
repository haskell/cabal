import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-build" ["script.hs"]
    cabalWithStdin "v2-repl" ["script.hs"] ""

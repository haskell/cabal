import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-run" ["script.hs"]
    cabal' "v2-run" ["script.hs"]

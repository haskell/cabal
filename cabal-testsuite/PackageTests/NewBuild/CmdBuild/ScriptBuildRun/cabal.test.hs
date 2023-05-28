import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-build" ["script.hs"]
    cabal' "v2-run" ["script.hs"]

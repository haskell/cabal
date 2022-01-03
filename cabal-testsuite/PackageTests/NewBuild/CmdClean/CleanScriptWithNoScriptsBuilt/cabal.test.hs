import Test.Cabal.Prelude

main = cabalTest . void $ do
    cabal' "v2-clean" []
    cabal' "v2-clean" ["script.hs"]

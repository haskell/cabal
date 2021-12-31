import Test.Cabal.Prelude

main = cabalTest $ do
    void . fails $ cabal' "v2-run" ["./Main.hs"]

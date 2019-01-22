import Test.Cabal.Prelude

main = cabalTest $ do
    expectBroken 5775 $ cabal' "v2-run" ["script.hs"]
    return ()

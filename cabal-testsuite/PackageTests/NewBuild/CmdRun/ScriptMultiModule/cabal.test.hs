import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "v2-run" ["script.hs"]
    assertOutputContains "Hello World" res

import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
    res <- cabal' "v2-run" ["script.lhs"]
    assertOutputContains "Hello World" res

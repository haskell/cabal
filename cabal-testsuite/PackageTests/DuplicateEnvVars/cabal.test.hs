import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
    res <- cabal' "test" ["all"]
    assertOutputContains "No duplicate environment variables found" res

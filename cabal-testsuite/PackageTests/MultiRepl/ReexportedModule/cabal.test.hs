import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
    -- For the multi-repl command
    good_ver <- isGhcVersion ">= 9.12"
    skipUnlessAnyCabalVersion ">= 3.15"
    if good_ver
      then do
        res <- cabalWithStdin "v2-repl" ["all"] ""
        assertOutputContains "Ok, two" res
      else do
        res <- fails $ cabalWithStdin "v2-repl" ["all"] ""
        assertOutputContains "Multi-repl does not work with complicated" res


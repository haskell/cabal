import Test.Cabal.Prelude

main = do
  cabalTest $ do
    -- custom-setup stanza is not supported by Cabal's bundled with GHC's before 8.8
    skipUnlessGhcVersion ">= 8.8"
    skipIfGhcVersion ">= 9.10"
    res <- cabalWithStdin "v2-repl" ["foo"] ""
    assertOutputContains "- foo-0.1.0.0 (interactive)" res

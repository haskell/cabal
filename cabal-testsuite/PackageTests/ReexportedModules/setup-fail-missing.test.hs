import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 7.9"
    withDirectory "p-fail-missing" $ do
        r <- fails $ setup' "configure" []
        assertOutputContains "Missing" r

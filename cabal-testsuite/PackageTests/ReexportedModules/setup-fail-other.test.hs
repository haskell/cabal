import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 7.9"
    withDirectory "p-fail-other" $ do
        r <- fails $ setup' "configure" []
        assertOutputContains "Private" r

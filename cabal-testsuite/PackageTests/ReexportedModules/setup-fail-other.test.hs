import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 7.9"
    withDirectory "p" $ do
        r <- fails $ setup' "configure" ["--cabal-file", "p.cabal.fail-other"]
        assertOutputContains "Private" r

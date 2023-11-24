import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    r <- fails $ withDirectory "Includes2-fail" $ setup' "configure" []
    assertOutputContains "mysql" r

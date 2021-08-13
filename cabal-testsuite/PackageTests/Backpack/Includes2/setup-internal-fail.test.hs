import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    r <- fails $ setup' "configure" ["--cabal-file", "Includes2.cabal.fail"]
    assertOutputContains "mysql" r

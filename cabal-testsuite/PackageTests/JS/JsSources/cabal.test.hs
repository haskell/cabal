import Test.Cabal.Prelude

main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 9.6"
    skipUnless =<< isJavaScript

    res <- cabal' "v2-run" ["demo"]
    assertOutputContains "Hello JS!" res

import Test.Cabal.Prelude

main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 9.6"
    skipUnlessJavaScript
    skipIfWindows

    res <- cabal' "v2-run" ["demo"]
    assertOutputContains "Hello JS!" res

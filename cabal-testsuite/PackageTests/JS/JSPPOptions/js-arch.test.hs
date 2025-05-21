import Test.Cabal.Prelude

main = do
    skipUnlessJavaScript
    skipIfWindows ""
    setupAndCabalTest $ do
        skipUnlessGhcVersion ">= 9.12"
        res <- cabal' "v2-run" ["demo"]
        assertOutputContains "Hello definition!" res

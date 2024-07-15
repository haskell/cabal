import Test.Cabal.Prelude

main = do
    skipUnlessJavaScript
    skipIfWindows ""
    setupAndCabalTest $ do
        skipUnlessGhcVersion ">= 9.6"
        res <- cabal' "v2-run" ["demo"]
        assertOutputContains "Hello JS!" res

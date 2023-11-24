import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 7.9"
    withPackageDb $ do
        withDirectory "containers-dupe" $
            setup_install []
        withDirectory "p-fail-ambiguous" $ do
            r <- fails $ setup' "configure" []
            assertOutputContains "Data.Map" r

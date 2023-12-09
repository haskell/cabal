import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withPackageDb $
        withDirectory "Includes2" $ do
            setup_install []
            -- TODO: haddock for internal method doesn't work
            runExe "exe" []

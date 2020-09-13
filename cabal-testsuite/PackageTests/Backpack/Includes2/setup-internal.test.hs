import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withPackageDb $ do
        setup_install ["--cabal-file", "Includes2.cabal"]
        -- TODO: haddock for internal method doesn't work
        runExe "exe" []

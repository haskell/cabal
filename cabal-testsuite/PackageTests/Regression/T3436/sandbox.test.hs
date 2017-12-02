import Test.Cabal.Prelude

-- Regression test for issue #3436
main = cabalTest $ do
    withSandbox $ do
        cabal "install" ["./Cabal-99998"]
        cabal_sandbox "add-source" ["Cabal-99999"]

        -- Install custom-setup, which has a setup dependency on Cabal-99999.
        -- cabal should build the setup script with Cabal-99999, but then
        -- configure should fail because Setup just prints an error message
        -- imported from Cabal and exits.
        r <- fails $ cabal' "install" ["custom-setup/"]
        assertOutputContains "This is Cabal-99999" r

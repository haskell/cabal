import Test.Cabal.Prelude

-- Regression test for issue #3436
--
-- #3436 occurred when a package with a custom setup specified a 'cabal-version'
-- that was newer than the version of the installed Cabal library, even though
-- the solver didn't choose the installed Cabal for the package's setup script.
--
-- This test installs a fake Cabal-1.2 and then tries to build the package
-- custom-setup, which depends on a fake Cabal-2.0 (through cabal-version and
-- setup-depends).
main = cabalTest $ do
    withSandbox $ do
        cabal "install" ["./Cabal-1.2"]
        cabal_sandbox "add-source" ["Cabal-2.0"]

        -- cabal should build custom-setup's setup script with Cabal-2.0, but
        -- then configure should fail because Setup just prints an error message
        -- imported from Cabal and exits.
        r <- fails $ cabal' "install" ["custom-setup/"]
        assertOutputContains "This is Cabal-2.0" r

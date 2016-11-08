import Test.Cabal.Prelude
-- On old versions of setup, an explicit dependency constraint which
-- doesn't match the internal library causes us to use external
-- library.  We got rid of this functionality in 1.25; now, we always
-- use internal, and just emit an error if you check.
main = setupAndCabalTest . withPackageDb $ do
    withDirectory "to-install" $ setup_install []
    setup_build []
    r <- runExe' "lemon" []
    assertEqual
        ("executable should have linked with the internal library")
        ("foo foo myLibFunc internal")
        (concatOutput (resultOutput r))

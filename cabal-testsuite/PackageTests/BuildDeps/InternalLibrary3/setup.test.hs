import Test.Cabal.Prelude
-- Test that sublibrary is preferred to an installed on
-- with the same name and LATER version
main = setupAndCabalTest . withPackageDb $ do
    withDirectory "to-install" $ setup_install []
    setup_build []
    r <- runExe' "lemon" []
    assertEqual
        ("executable should have linked with the sublibrary")
        ("foo foo myLibFunc internal")
        (concatOutput (resultOutput r))

import Test.Cabal.Prelude
-- Test to ensure that setup_macros.h are computed per-component.
main = setupAndCabalTest $ do
    setup_build ["--ipid", "macros-0.1.0.0-inplace"]
    runExe "macros-a" []
    runExe "macros-b" []


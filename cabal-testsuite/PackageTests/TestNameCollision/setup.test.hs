import Test.Cabal.Prelude
-- Test that if test suite has a name which conflicts with a package
-- which is in the database, we can still use the test case (they
-- should NOT shadow).
main = setupAndCabalTest $ do
    skipIfAllCabalVersion "< 2.2"
    withPackageDb $ do
        withDirectory "parent" $ setup_install []
        withDirectory "child" $ do
            setup_build ["--enable-tests"]
            setup "test" []

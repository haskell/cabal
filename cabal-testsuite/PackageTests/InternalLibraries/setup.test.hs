import Test.Cabal.Prelude
-- Basic test for sublibraries (in p); package q is to make
-- sure that the sublibrary correctly is used, not the
-- external library.
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "q" $ setup_install []
        withDirectory "p" $ do
            setup_install []
            setup "clean" []
            r <- runInstalledExe' "foo" []
            assertOutputContains "I AM THE ONE" r

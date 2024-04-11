import Test.Cabal.Prelude
main = cabalTest $ do
    noCabalPackageDb . withPackageDb $ do
        withDirectory "p-no-package-dbs" $ do
            res <- fails $ cabal' "v2-build" []
            assertOutputContains "No package databases have been specified." res

import Test.Cabal.Prelude
main = cabalTest $ do
    withPackageDb $ do
        withDirectory "p" $
            setup_install []

        withDirectory "q" $ do
            res <- fails $ cabal' "v2-build" []
            assertOutputContains "unknown package: p" res

import Test.Cabal.Prelude
main = cabalTest $ do
    recordMode DoNotRecord $ withRepo "repo" $ withPackageDb $ do
        withDirectory "p1" $
            setup_install []

        env <- getTestEnv
        let pkgDbPath = testPackageDbDir env

        withDirectory "p2" $ do
            void $ cabal' "v2-build" ["--package-db=" ++ pkgDbPath]

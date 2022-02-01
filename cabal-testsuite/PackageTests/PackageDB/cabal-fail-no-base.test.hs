import Test.Cabal.Prelude
main = cabalTest $ do
    withPackageDb $ do
        withDirectory "p" $
            setup_install []

        env <- getTestEnv
        let pkgDbPath = testPackageDbDir env

        withDirectory "q" $ do
            res <- fails $ cabal' "v2-build" ["--package-db=clear", "--package-db=" ++ pkgDbPath]
            assertOutputContains "unknown package: base" res

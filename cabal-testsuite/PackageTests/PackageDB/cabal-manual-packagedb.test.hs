import Test.Cabal.Prelude
main = cabalTest $ do
    withPackageDb $ do
        withDirectory "p" $
            setup_install []

        env <- getTestEnv
        let pkgDbPath = testPackageDbDir env
        withDirectory "q" $
            cabal "v2-build" [ "--package-db=clear"
                             , "--package-db=global"
                             , "--package-db=" ++ pkgDbPath]

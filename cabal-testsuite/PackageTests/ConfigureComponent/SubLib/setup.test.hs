import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    withPackageDb $ do
        setup_install ["sublib"]
        setup_install ["exe"]
        runExe' "exe" [] >>= assertOutputContains "OK"

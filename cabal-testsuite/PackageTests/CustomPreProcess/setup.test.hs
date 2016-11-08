import Test.Cabal.Prelude
-- Test internal custom preprocessor
main = setupAndCabalTest $ do
    setup_build []
    runExe' "hello-world" []
        >>= assertOutputContains "hello from A"

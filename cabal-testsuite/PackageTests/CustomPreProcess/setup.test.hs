import Test.Cabal.Prelude
-- Test internal custom preprocessor
main = setupTest $ do
    setup_build []
    runExe' "hello-world" []
        >>= assertOutputContains "hello from A"

import Test.Cabal.Prelude
-- Test internal custom preprocessor
main = setupTest $ do
    skipUnless =<< hasCabalForGhc
    setup_build []
    runExe' "hello-world" []
        >>= assertOutputContains "hello from A"

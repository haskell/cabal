import Test.Cabal.Prelude
-- Test internal custom preprocessor
main = setupTest $ do
    skipUnless "no Cabal for GHC" =<< hasCabalForGhc
    setup_build []
    runExe' "hello-world" []
        >>= assertOutputContains "hello from A"

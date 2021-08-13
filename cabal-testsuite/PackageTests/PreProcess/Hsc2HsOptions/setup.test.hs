import Test.Cabal.Prelude
-- Check that preprocessors (hsc2hs) are run
main = setupAndCabalTest $ do
    setup_build []
    r <- runExe' "my-executable" []
    assertOutputContains "hsc2hs value: 42" r
    assertOutputContains "ghc value: 0" r

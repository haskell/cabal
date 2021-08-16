import Test.Cabal.Prelude
-- Test unneed version bound on internal build-tools deps
main = setupAndCabalTest . expectBroken 7470 $ do
    setup' "configure" []
    assertOutputContains "extraneous version range"
        =<< setup' "sdist" []

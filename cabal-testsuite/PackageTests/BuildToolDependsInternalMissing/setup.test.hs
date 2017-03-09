import Test.Cabal.Prelude
-- Test impossible version bound on internal build-tools deps
main = setupAndCabalTest $ do
    assertOutputContains "missing internal executable"
        =<< fails (setup' "configure" [])

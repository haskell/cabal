import Test.Cabal.Prelude
-- Test unneed version bound on internal build-tools deps
main = setupAndCabalTest $ do
    setup' "configure" []
    -- Hack alert: had to squelch this warning because of #5119.
--    assertOutputContains "extraneous version range"
--        =<< setup' "sdist" []
    return ()

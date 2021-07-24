import Test.Cabal.Prelude
-- Test unneed version bound on internal build-tools deps
main = setupAndCabalTest $ do
    setup' "configure" []
    -- this specific warning was commented out, this check is inverted so it doesn't break the CI
    assertOutputDoesNotContain "extraneous version range"
        =<< setup' "sdist" []

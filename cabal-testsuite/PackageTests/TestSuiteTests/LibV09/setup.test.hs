import Test.Cabal.Prelude
-- Test if detailed-0.9 builds correctly
main = setupAndCabalTest $ do
    skipIfAllCabalVersion "< 1.20"
    setup_build ["--enable-tests"]

import Test.Cabal.Prelude
-- Test if detailed-0.9 builds correctly
main = setupAndCabalTest $ do
    skipUnless "no Cabal for GHC" =<< hasCabalForGhc
    setup_build ["--enable-tests"]

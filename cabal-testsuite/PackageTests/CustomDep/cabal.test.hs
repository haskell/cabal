import Test.Cabal.Prelude
main = cabalTest $ do
    -- NB: This variant seems to use the bootstrapped Cabal?
    skipUnless =<< hasCabalForGhc
    -- TODO: Hack, delete me
    withEnvFilter (/= "HOME") $ do
        cabal "new-build" []

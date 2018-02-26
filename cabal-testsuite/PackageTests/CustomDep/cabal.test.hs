import Test.Cabal.Prelude
main = cabalTest $ do
    -- NB: This variant seems to use the bootstrapped Cabal?
    skipUnless =<< hasCabalForGhc
    -- implicit setup-depends conflict with GHC >= 8.2; c.f. #415
    skipIf =<< (ghcVersionIs (>= mkVersion [8,2]))
    -- This test depends heavily on what packages are in the global
    -- database, don't record the output
    recordMode DoNotRecord $ do
        -- TODO: Hack, delete me
        withEnvFilter (/= "HOME") $ do
            cabal "new-build" ["all"]

import Test.Cabal.Prelude
main = cabalTest $ do
    -- implicit setup-depends conflict with GHC >= 8.2; c.f. #415
    skipIf =<< (ghcVersionIs (>= mkVersion [8,2]))
    -- Regression test for #4393
    recordMode DoNotRecord $ do
        -- TODO: Hack; see also CustomDep/cabal.test.hs
        withEnvFilter (/= "HOME") $ do
            -- On -v2, we don't have vQuiet set, which suppressed
            -- the error
            cabal "new-build" ["-v1"]

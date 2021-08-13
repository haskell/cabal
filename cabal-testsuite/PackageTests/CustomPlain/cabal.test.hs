import Test.Cabal.Prelude
main = cabalTest $ do
    -- implicit setup-depends conflict with GHC >= 8.2; c.f. #415
    skipUnlessGhcVersion "< 8.2"
    -- Regression test for #4393
    recordMode DoNotRecord $ do
        -- TODO: Hack; see also CustomDep/cabal.test.hs
        withEnvFilter (`notElem` ["HOME", "CABAL_DIR"]) $ do
            -- On -v2, we don't have vQuiet set, which suppressed
            -- the error
            cabal "v2-build" ["-v1"]

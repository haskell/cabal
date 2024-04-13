import Test.Cabal.Prelude
main = cabalTest $ do
    -- implicit setup-depends conflict with GHC >= 8.2; c.f. #415
    skipUnlessGhcVersion "< 8.2"
    -- This test depends heavily on what packages are in the global
    -- database, don't record the output
    recordMode DoNotRecord $ do
        -- TODO: Hack, delete me
        withEnvFilter (`notElem` ["HOME", "CABAL_DIR"]) $ do
            cabal "v2-build" ["all"]

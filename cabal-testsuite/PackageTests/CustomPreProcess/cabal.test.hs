import Test.Cabal.Prelude
-- Test internal custom preprocessor
main = cabalTest $ do
    skipUnless "no Cabal for GHC" =<< hasCabalForGhc

    -- old Cabal's ./Setup.hs output is difficult to normalise
    recordMode DoNotRecord $
        cabal "v2-build" []

    -- here, we only care that result works:
    withPlan $ do
        r <- runPlanExe' "internal-preprocessor-test" "hello-world" []
        assertOutputContains "hello from A" r

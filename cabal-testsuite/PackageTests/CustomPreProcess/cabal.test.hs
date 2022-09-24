import Test.Cabal.Prelude
-- Test internal custom preprocessor
main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    skipUnless "no Cabal for GHC" =<< hasCabalForGhc

    -- old Cabal's ./Setup.hs output is difficult to normalise
    recordMode DoNotRecord $
        cabal "v2-build" []

    -- here, we only care that result works:
    withPlan $ do
        r <- runPlanExe' "internal-preprocessor-test" "hello-world" []
        assertOutputContains "hello from A" r

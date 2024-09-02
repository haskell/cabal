import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    expectBrokenIfWindowsCI 10191 $ withProjectFile "cabal.internal.project" $ do
        cabal "v2-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "I" "exe" []
            assertOutputContains "fromList [(0,2),(2,4)]" r

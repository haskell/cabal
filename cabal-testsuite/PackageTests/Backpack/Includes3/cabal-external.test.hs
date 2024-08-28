import Test.Cabal.Prelude

main = cabalTest $ do
    ghcVer <- isGhcVersion ">= 9.10"
    skipUnlessGhcVersion ">= 8.1"
    expectBrokenIf (isWindows && ghcVer) 10191 $ withProjectFile "cabal.external.project" $ do
        cabal "v2-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "exe" "exe" []
            assertOutputContains "fromList [(0,2),(2,4)]" r

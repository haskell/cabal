import Test.Cabal.Prelude

main = cabalTest $ do
    ghcVer <- isGhcVersion ">= 9.10"
    skipUnlessGhcVersion ">= 8.1"
    skipIf "Windows + 9.10.1 (#10191)" (isWindows && ghcVer)
    withProjectFile "cabal.external.project" $ do
        cabal "v2-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "exe" "exe" []
            assertOutputContains "fromList [(0,2),(2,4)]" r

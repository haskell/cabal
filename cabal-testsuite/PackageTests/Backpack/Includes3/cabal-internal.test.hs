import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    skipIfWindows -- TODO: https://github.com/haskell/cabal/issues/6271
    withProjectFile "cabal.internal.project" $ do
        cabal "v2-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "Includes3" "exe" []
            assertOutputContains "fromList [(0,2),(2,4)]" r

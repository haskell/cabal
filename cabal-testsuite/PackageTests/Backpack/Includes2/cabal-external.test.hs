import Test.Cabal.Prelude

main = do
  cabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    withProjectFile "cabal.external.project" $ do
        cabal "v2-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "exe" "exe" []
            assertOutputContains "minemysql minepostgresql" r

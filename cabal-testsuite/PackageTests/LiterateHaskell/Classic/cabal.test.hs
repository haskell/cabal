import Test.Cabal.Prelude

main = cabalTest $ do
    res <- cabal' "v2-build" ["."]
    withPlan $ runPlanExe "test" "foo" []

import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-build" ["foreign-opts-c-exe"]
    withPlan $ runPlanExe "foreign-opts-c" "foreign-opts-c-exe" []

import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-build" ["foreign-opts-capi-exe"]
    withPlan $ runPlanExe "foreign-opts-capi" "foreign-opts-capi-exe" []

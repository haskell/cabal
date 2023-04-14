import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-build" ["foreign-opts-cxx-exe"]
    withPlan $ runPlanExe "foreign-opts-cxx" "foreign-opts-cxx-exe" []

import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "v2-build" ["cpp-options-only-for-hs-exe"]
    withPlan $ runPlanExe "cpp-options-only-for-hs" "cpp-options-only-for-hs-exe" []

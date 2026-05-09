import Test.Cabal.Prelude

main = cabalTest $ recordMode DoNotRecord $ do
  cabal "v2-build" ["cpp-opts-exe"]
  withPlan $ runPlanExe "cpp-opts" "cpp-opts-exe" []

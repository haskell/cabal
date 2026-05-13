import Test.Cabal.Prelude

main = do
  cabalTest $ recordMode DoNotRecord $ do
    cwd <- fmap testCurrentDir getTestEnv
    let wrapper = cwd </> "scripts/as-wrapper.sh"
    cabal "v2-build" ["foreign-opts-pgma-exe"]
    withPlan $ runPlanExe "foreign-opts-pgma" "foreign-opts-pgma-exe" []

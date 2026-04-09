import Test.Cabal.Prelude

main = do
  skipIfWindows "requires a POSIX shell script as the C compiler wrapper"
  cabalTest $ do
    skipUnlessGhcVersion ">= 9.4"
    cwd <- fmap testCurrentDir getTestEnv
    let wrapper = cwd </> "scripts/cc-wrapper.sh"
    cabal "v2-build" ["foreign-opts-pgmc-exe"]
    withPlan $ runPlanExe "foreign-opts-pgmc" "foreign-opts-pgmc-exe" []

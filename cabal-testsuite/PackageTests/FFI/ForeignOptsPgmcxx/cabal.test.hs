import Test.Cabal.Prelude

main = do
  skipIfWindows "requires a POSIX shell script as the C++ compiler wrapper"
  -- The assumption that the C++ compiler is part of the toolchain is only since ghc-9.4.
  cabalTest $ recordMode DoNotRecord $ do
    skipUnlessGhcVersion ">= 9.4"
    cwd <- fmap testCurrentDir getTestEnv
    let wrapper = cwd </> "scripts/cxx-wrapper.sh"
    cabal "v2-build" ["foreign-opts-pgmcxx-exe"]
    withPlan $ runPlanExe "foreign-opts-pgmcxx" "foreign-opts-pgmcxx-exe" []

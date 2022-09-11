import Test.Cabal.Prelude
main = cabalTest $ do
  isWin <- isWindows
  ghc94 <- isGhcVersion "== 9.4.*"
  expectBrokenIf (isWin && ghc94) 8451 $ do
    cabal "v2-build" ["foreign-opts-c-exe"]
    withPlan $ runPlanExe "foreign-opts-c" "foreign-opts-c-exe" []

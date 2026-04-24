import Test.Cabal.Prelude

main = do
  skipIfWindows "requires a POSIX shell script as the C compiler wrapper"
  -- Use -pgmc to ensure that Cabal always passes cc-options, ld-options to GHC (#4435, #9801)
  -- We can only do this on GHC >= 9.4, as we need https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6949
  -- Without that GHC MR, this change would cause GHC to never pass -no-pie when linking,
  -- which can cause breakage depending on the C toolchain use. We would have to appropriately
  -- pass -pgmc-supports-no-pie as appropriate to avoid this regression.
  cabalTest $ recordMode DoNotRecord $ do
    skipUnlessGhcVersion ">= 9.4"
    cabal "v2-build" ["foreign-opts-pgmc-exe"]
    withPlan $ runPlanExe "foreign-opts-pgmc" "foreign-opts-pgmc-exe" []

import Test.Cabal.Prelude

-- ForeignOptsPgml, cabal (v2) variant: an explicitly passed -pgml has to
-- drive GHC's final link, taking precedence over the -pgml that Cabal
-- injects itself (user ghc-options come last on the GHC command line).
-- The wrapper redirects an FFI call with -Wl,--wrap; if GHC's link is not
-- driven by the wrapper, the executable fails at runtime.
--
-- Furthermore, whether the linker supports -no-pie is probed by Cabal
-- (like GHC's build system probes its own C compiler): the probe of the
-- resolved C compiler succeeds, so -pgml-supports-no-pie is passed after
-- -pgml.
main = do
  skipIfWindows "requires a POSIX shell script as the compiler wrapper"
  skipIfOSX "ld64 does not support --wrap"
  cabalTest $ recordMode DoNotRecord $ do
    -- Cabal only passes -pgml (like -pgmc) on GHC >= 9.4, see the
    -- ForeignOptsPgmc test for the rationale.
    skipUnlessGhcVersion ">= 9.4"
    env <- getTestEnv
    let wrapper = testCurrentDir env </> "scripts" </> "cc-wrapper.sh"
    res <- cabal' "v2-build" ["-v2", "--ghc-options=-pgml " ++ wrapper, "foreign-opts-pgml-exe"]
    assertOutputContains "-pgml-supports-no-pie" res
    withPlan $ do
      resExe <- runPlanExe' "foreign-opts-pgml" "foreign-opts-pgml-exe" []
      assertOutputContains "The secret is 66" resExe

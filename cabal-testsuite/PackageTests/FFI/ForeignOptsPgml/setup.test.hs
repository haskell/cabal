import Test.Cabal.Prelude

-- ForeignOptsPgml, setup (v1) variant: the C compiler resolved by Cabal
-- (--with-gcc) has to drive GHC's final link, which is how Cabal passes
-- -pgml to GHC; the wrapper redirects an FFI call with -Wl,--wrap. If
-- GHC's link is not driven by the wrapper, the executable fails at
-- runtime.
main = do
  skipIfWindows "requires a POSIX shell script as the compiler wrapper"
  skipIfOSX "ld64 does not support --wrap"
  setupTest $ recordMode DoNotRecord $ do
    -- Cabal only passes -pgml (like -pgmc) on GHC >= 9.4, see the
    -- ForeignOptsPgmc test for the rationale.
    skipUnlessGhcVersion ">= 9.4"
    env <- getTestEnv
    let wrapper = testCurrentDir env </> "scripts" </> "cc-wrapper.sh"
        noPieWrapper = testCurrentDir env </> "scripts" </> "no-pie-wrapper.sh"
    setup "configure" ["--with-gcc=" ++ wrapper]
    res <- setup' "build" []
    -- The probe of the wrapper compiler (which drives the link, as it is
    -- also passed as -pgml) finds -no-pie support.
    assertOutputContains "-pgml-supports-no-pie" res
    resExe <- runExe' "foreign-opts-pgml-exe" []
    assertOutputContains "The secret is 66" resExe
    -- A linker driver without -no-pie support (emulated by
    -- scripts/no-pie-wrapper.sh, which also defaults to non-PIE output) is
    -- not claimed to support it, so -pgml-supports-no-pie is not passed;
    -- GHC's link still succeeds without the flag.
    setup "configure" ["--with-gcc=" ++ noPieWrapper]
    resNoPie <- setup' "build" ["-v2"]
    assertOutputContains "-pgml " resNoPie
    assertOutputDoesNotContain "-pgml-supports-no-pie" resNoPie

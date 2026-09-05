import Test.Cabal.Prelude

-- Regression test for #10789: inconsistent use of `ld-options`.
--
-- Project-level `ld-options` (`--ld-options`) are documented as flags for
-- GHC's linking phase: they are passed to GHC as `-optl` arguments, which
-- forwards them to the C compiler driver acting as the linker. Cabal also
-- invokes the `ld` program directly (when probing its capabilities and when
-- combining object files into a library for GHCi); previously the user's
-- `-Wl,`-prefixed options were passed to `ld` verbatim, which broke the
-- probes (silently disabling `--enable-library-for-ghci`) and could fail
-- the build, while the very same options worked fine when passed to GHC.
main = do
    -- The assertions below require a linker that reports support for
    -- relocatable output; `lld` (Windows) and `ld64` (macOS) do not.
    skipIfWindows "lld does not support relocatable output"
    skipIfOSX "ld64 does not support relocatable output"
    cabalTest $ recordMode DoNotRecord $ do
        -- The `-Wl,`-prefixed `ld-options` must not break the `ld`
        -- capability probes: the library for GHCi has to be built.
        cabal "v2-build" ["--enable-library-for-ghci", "all"]
        _ <- assertGlobMatchesTestDir testDistDir "**/HSt10789-0.1-inplace.o"

        -- The same `ld-options` must still reach the linker through GHC
        -- (as `-optl` flags).
        withPlan $ do
            res <- runPlanExe' "t10789" "wrap-exe" []
            assertOutputContains "The secret is 55" res

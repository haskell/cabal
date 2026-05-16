# ForeignOptsPgmc

This test case asserts that cabal passes the `-pgmc` GHC option to override the C compiler program.

The cabal file sets `ghc-options: -pgmc scripts/cc-wrapper.sh`, pointing GHC at a shell script wrapper (`scripts/cc-wrapper.sh`) instead of the system C compiler. The wrapper adds `-D__TESTOPT_PGMC__=66` to every compilation and then delegates to the real `cc`. The C source requires `__TESTOPT_PGMC__` to be defined; if the wrapper is not used as the C compiler, the build fails with a `#error`.

This test is skipped on Windows (no POSIX shell).

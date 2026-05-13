# ForeignOptsPgma

This test case asserts that cabal passes the `-pgma` GHC option to override the assembler program.

The cabal file sets `ghc-options: -pgma scripts/as-wrapper.sh`, pointing GHC at a shell script wrapper (`scripts/as-wrapper.sh`) instead of the system assembler program. The wrapper adds `--defsym meaning_of_life_val=33` to every compilation and then delegates to the real `as`. The assembler source requires `meaning_of_life_val` to be defined; if the wrapper is not used as the assembler program, the build fails.

This test is skipped on Windows (no POSIX shell).

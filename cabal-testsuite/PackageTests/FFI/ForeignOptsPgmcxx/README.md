# ForeignOptsPgmcxx

This test case asserts that cabal passes the `-pgmcxx` GHC option to override the C++ compiler program.

The cabal file sets `ghc-options: -pgmcxx scripts/cxx-wrapper.sh`, pointing GHC at a shell script wrapper (`scripts/cxx-wrapper.sh`) instead of the system C++ compiler. The wrapper adds `-D__TESTOPT_PGMCXX__=67` to every compilation and then delegates to the real `g++`. The C++ source requires `__TESTOPT_PGMCXX__` to be defined; if the wrapper is not used as the C++ compiler, the build fails with a `#error`.

This test is skipped on Windows (no POSIX shell).

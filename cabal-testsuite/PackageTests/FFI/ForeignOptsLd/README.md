# ForeignOptsLd

This test case asserts that cabal passes `ld-options` to the linker.

The test uses `ld-options: -Wl,--wrap=meaning_of_life_ld_real` which instructs the GNU linker to redirect all calls to `meaning_of_life_ld_real` to `__wrap_meaning_of_life_ld_real` instead. The real function returns 0; the wrapper function returns 55. If `ld-options` were not passed, the real function would be called and the test would fail with an unexpected value.

This test is skipped on macOS (which uses Apple's linker, not GNU ld) and on Windows.

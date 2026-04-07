# ForeignOptsAsm

This test case asserts that cabal passes `asm-options` to the assembler when compiling assembly source files.

The test uses `asm-options: -Wa,--defsym,meaning_of_life_val=33` which routes through GHC's `-opta` flag to `gcc`, which then passes `--defsym meaning_of_life_val=33` to the actual GNU assembler. The assembly source file references `meaning_of_life_val` as an immediate operand; if the define is not passed, the assembler will fail with an undefined symbol error.

This test is skipped on macOS (which uses Apple's assembler, not GNU as) and on non-x86_64/aarch64 architectures.

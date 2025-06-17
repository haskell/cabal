---
synopsis: Add --with-repl flag to specify alternative REPL program
packages: [cabal-install, Cabal]
prs: [10996]
issues: [9115]
---

Added a new `--with-repl` command-line option that allows specifying an alternative
program to use when starting a REPL session, instead of the default GHC.

This is particularly useful for tools like `doctest` and `hie-bios` that need to
intercept the REPL session to perform their own operations. Previously, these tools
had to use `--with-ghc` which required them to proxy all GHC invocations, including
dependency compilation, making the implementation more complex.

The `--with-repl` option only affects the final REPL invocation, simplifying the
implementation of such wrapper tools.

Example usage:
```bash
cabal repl --with-repl=doctest
cabal repl --with-repl=/path/to/custom/ghc
```

This change also removes the special handling for response files with `--interactive`
mode, as tools are now expected to handle response files appropriately.

---
synopsis: Add --enable-library-bytecode flag
packages: [Cabal, cabal-install]
prs: 4560
issues: 11188
significance: significant
---

Introduce the `--enable-library-bytecode` flag to build bytecode libraries
next to the usual object and shared artifacts. Bytecode libraries are useful
when asking GHCi to use bytecode for library dependencies (e.g. `-fprefer-byte-code`) and are
currently only available with GHC 9.15 or newer; the flag is ignored on older
compilers.

Implements the Cabal Proposal: [Cabal Support for Bytecode Objects and Bytecode Libraries](https://github.com/haskell/cabal-proposals/blob/master/proposals/bytecode-files.md).

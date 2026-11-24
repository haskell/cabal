---
synopsis: Report the configured compiler in build-info, `cabal path` and multi-repl
packages: [Cabal, cabal-install]
prs: 12340
---

The build-info JSON, `cabal path --compiler-info` and `cabal repl` with several
targets now use the compiler program found when configuring the compiler,
instead of looking up a program by the compiler's flavour. `cabal path` no
longer fails with "Unsupported compiler flavour" for a compiler other than GHC,
and the `UnknownCompilerFlavor` constructor of `CabalException` (error code
3102) is removed.

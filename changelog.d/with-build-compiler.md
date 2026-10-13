---
synopsis: Add --with-build-compiler for selecting the build-stage compiler
packages: [cabal-install]
prs: 12356
significance: significant
---

`cabal` can now be told which compiler to use for the *build* stage of a build —
the compiler that compiles custom `Setup.hs` scripts and build-tool
dependencies — separately from the compiler that produces the artifacts
(the *host* stage). This is a step towards cross-compilation support.

- New command-line flag `--with-build-compiler` (alias `--with-build-hc`) and
  `--with-build-hc-pkg`.
- New `cabal.project` fields `build-compiler`, `with-build-compiler` and
  `with-build-hc-pkg`.

When no build compiler is given, the build stage uses the host compiler, so
ordinary (non-cross) builds are unaffected.

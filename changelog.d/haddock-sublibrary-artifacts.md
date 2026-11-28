---
synopsis: Reuse component compilation artifacts when building Haddocks
packages: [Cabal]
prs: [12491]
---

Use the directory each component was compiled in when reusing compilation
artifacts for Haddock. This lets Haddock find sublibrary interfaces and use the
original stub and HIE directories, avoiding unnecessary recompilation when the
project has already been built with documentation enabled. Executables, test
suites and benchmarks, which are compiled into a `<name>-tmp` subdirectory of
their build directory, are handled the same way, and documenting them no longer
writes HIE files outside that directory.

Add `componentArtifactsDir` to `Distribution.Simple.LocalBuildInfo`.

---
synopsis: Reuse sublibrary compilation artifacts when building Haddocks
packages: [Cabal]
prs: [12491]
---

Use each component's build directory when reusing compilation artifacts for
Haddock. This lets Haddock find sublibrary interfaces and use the original
stub and HIE directories, avoiding unnecessary recompilation when the project
has already been built with documentation enabled.

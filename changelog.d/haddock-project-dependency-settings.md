---
synopsis: Reuse the same dependency instances throughout haddock-project
packages: [cabal-install]
prs: [12491]
---

`cabal haddock-project` now uses the same documentation settings while planning,
building dependencies, generating documentation, and collecting interfaces.
Previously, these phases could select different store instances of dependencies,
causing repeated package reconfiguration and recompilation even when project
sources and configuration were unchanged.

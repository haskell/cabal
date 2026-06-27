---
synopsis: Add program prefix and suffix to `BuildOptions`
packages: [Cabal, Cabal-hooks]
prs: 11787
issues: 11168
---

Two new fields of `BuildOptions`, `programPrefix` and `programSuffix`, have
been added. This allows the installed executable program prefix and suffix to
be set by `SetupHooks` for packages with `build-type: Hooks`.

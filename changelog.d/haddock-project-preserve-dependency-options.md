---
synopsis: Preserve dependency documentation settings in haddock-project
packages: [cabal-install]
prs: [12491]
---

`cabal haddock-project` now uses one project context for planning, building,
generating documentation, and collecting interfaces. Its output options apply
to local project packages; dependencies keep their documentation and Haddock
settings from project and global configuration. This lets a preceding
`cabal build` and `haddock-project` reuse the same dependency instances.

Ordinary `cabal build` and `cabal haddock` commands continue to propagate explicit
documentation and Haddock flags to dependencies, including Hoogle and quick-jump.
To prepare dependency documentation for `haddock-project`, enable the desired
options under `package *` or the relevant dependency's package stanza. Existing
dependency documentation is copied when available; missing documentation is
reported without enabling it automatically.

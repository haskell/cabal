---
synopsis: Preserve dependency settings in haddock-project
packages: [cabal-install]
prs: [12667]
---

`cabal haddock-project` now applies its documentation and Haddock output
options only to local project packages. Dependencies keep their documentation
and Haddock settings from project and global configuration, the same while
planning, building dependencies, generating documentation, and collecting
interfaces. Previously, these phases could select different store instances of
dependencies, causing repeated package reconfiguration and recompilation even
when project sources and configuration were unchanged. A preceding
`cabal build` and `haddock-project` now reuse the same dependency instances.

Ordinary `cabal build` and `cabal haddock` commands continue to propagate explicit
documentation and Haddock flags to dependencies, including Hoogle and quick-jump.
To prepare dependency documentation for `haddock-project`, enable the desired
options under `package *` or the relevant dependency's package stanza. Existing
dependency documentation is copied when available; missing documentation is
reported without enabling it automatically.

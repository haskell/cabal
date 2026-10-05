---
synopsis: Do not run Haddock for components added to a multi-repl session
packages: [cabal-install]
prs: XXXXX
issues: 12242
---

With `documentation: True`, `cabal repl --enable-multi-repl` ran Haddock for
components that were added to the session only to satisfy the closure property.
Those components are built in memory, so Haddock failed with `Cabal-4569`.
Such components no longer build documentation.

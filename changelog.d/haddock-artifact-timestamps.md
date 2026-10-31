---
synopsis: Preserve compilation artifact timestamps when generating Haddock documentation
packages: [Cabal]
prs: [12491]
---

When reusing GHC compilation artifacts for Haddock, Cabal now preserves their
modification times while copying them to temporary directories. This prevents
copied `.hi` files from making existing `.hie` files appear outdated and
triggering unnecessary recompilation.

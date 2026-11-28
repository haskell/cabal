---
synopsis: Keep the build cache valid after `cabal haddock`
packages: [cabal-install]
prs: [12667]
---

`cabal haddock` and `cabal repl` no longer record their runs in the build
cache, as they do not complete a build. Previously, `cabal haddock` recorded an
empty set of built components for its targets, so the next `cabal build`
rebuilt them as "additional components to build". A repl invalidates the build
cache instead if it replaced compiler artifacts, e.g. with `-fobject-code`.

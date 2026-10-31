---
synopsis: Share builds between `cabal build` and `cabal haddock-project`
packages: [cabal-install]
prs: [12491]
---

The build phase of `cabal haddock-project` now uses the same settings as
`cabal build`, so that each can reuse the builds of the other: the Haddock
output options of `haddock-project` only apply to its Haddock phase, and it only
enables documentation when the project does not already enable it.

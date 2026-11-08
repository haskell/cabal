---
synopsis: Give the setup of a component each datadir variable once
packages: [cabal-install]
prs: 0000
issues: 12544
---

The environment overrides that cabal-install passes to the setup of each
component listed the datadir variable of a local package once for every
component of it in the plan, and every configured program kept a copy of the
list in its environment. In a project with many components this made each
`setup-config` grow with the square of the number of components.

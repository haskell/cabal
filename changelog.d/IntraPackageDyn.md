---
synopsis: Build dynamic libraries when needed for intra-package dependencies
packages: [Cabal]
prs: 11791
issues: 7684
---

When building a package with a sublibrary, Cabal now properly takes into account
intra-package dependencies when deciding whether each sublibrary should be built
in the dynamic way (e.g. because another library that depends on it uses
TemplateHaskell or QuasiQuotes).

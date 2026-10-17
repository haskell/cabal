---
synopsis: Plan caches no longer hold a GenericPackageDescription per component
packages: [cabal-install]
prs: 12464
issues: 12463
---

The in-library configure reads it from the package's `.cabal` file. With 80 public
sublibraries, `improved-plan` falls from 27 MB to 15 MB.

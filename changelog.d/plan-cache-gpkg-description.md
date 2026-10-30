---
synopsis: Plan caches no longer hold a GenericPackageDescription per component
packages: [cabal-install]
prs: 12464
issues: 12463
---

The original generic package description is now shared per package in the build
plan, rather than duplicated for every component. This reduces the size of the
plan and per-component configuration caches for packages with many sublibraries.

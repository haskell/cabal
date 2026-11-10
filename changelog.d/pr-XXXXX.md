---
synopsis: Cache one copy of the install plan
packages: [cabal-install]
prs: XXXXX
issues: 12463
---

The `improved-plan` cache file no longer holds two more copies of the install
plan, only which of the plan's packages are in the store.

---
synopsis: Fix unsolvable plans on compilers without wired-in unit ids
packages: [cabal-install]
prs: 12206
significance: normal
---

`dependOnWiredIns` added a `base >= 4.22` lower-bound constraint
unconditionally, even for compilers that report no wired-in unit ids at all
(GHC < 9.14). Such a compiler's installed `base` never satisfies that bound,
so any plan reaching this code path — which `allow-boot-library-installs`
alone is enough to trigger — became unsolvable. The constraint is now guarded
on the compiler actually reporting wired-in unit ids, matching the intent of
the surrounding comment.

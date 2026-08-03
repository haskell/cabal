---
synopsis: Fix `--allow-boot-library-installs` on GHC < 9.14
packages: [cabal-install]
prs: 12301
issues: 12328
---

`--allow-boot-library-installs` made builds fail on compilers that report no
wired-in unit ids, because `base` was constrained to version 4.22 or later
regardless of the compiler.

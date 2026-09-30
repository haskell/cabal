---
synopsis: Make 3.20 the latest known .cabal specification version
packages: [Cabal-syntax, cabal-install]
prs: 12275
issues: 12271
significance: significant
---

`cabalSpecLatest` was left at `CabalSpecV3_16` when `CabalSpecV3_18` and
`CabalSpecV3_20` were introduced, so the newest specification version the
library knew about lagged two releases behind the tree. It now points at
`CabalSpecV3_20`, the version under development.

`cabal-install` treated a `build-type: Simple` package whose `cabal-version`
exceeded `cabalSpecLatest` as a future-format package. It then tried to compile
an external `Setup.hs` without a Cabal setup dependency. Consequently, packages
that declared the released `cabal-version: 3.18` could fail to build. Affected
packages now use the in-process setup path.

Two further changes come with the bump:

- `cabalSpecVersionToSPDXListVersion` gained an explicit `CabalSpecV3_20` case,
  so standalone `Distribution.SPDX` parsing (and Cabal-QuickCheck's generators)
  now default to SPDX license list 3.28 rather than 3.26, matching released
  Cabal 3.18. Without the new case the bump would have fallen through to the
  catch-all and silently downgraded that default to 3.0 instead.

- `cabal.project` files are parsed at `cabalSpecLatest`, so the
  `deprecatedSince CabalSpecV3_20` marker on `prefer-oldest` becomes reachable
  for the first time: using that field now warns and points at `prefer-version`.

This does not change which `cabal-version` values a `.cabal` file may declare —
the parser accepts any version known to `cabalSpecFromVersionDigits`
independently of `cabalSpecLatest`.

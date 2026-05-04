---
synopsis: Caching of InstalledPackageIndex
packages: [Cabal, cabal-install]
prs: 11767
---

`cabal-install` now keeps a running `InstalledPackageIndex` that it updates
as packages in a project get built. This allows skipping expensive `ghc-pkg`
calls when configuring each package (within `computePackageInfo` in the
Cabal `configure` function).

To enable this, a new function in the `Cabal` library, `computePackageInfoFromIndex`,
has been extracted from `computePackageInfo` function `Distribution.Simple.Configure`.
This allows passing an explicit `InstalledPackageIndex` instead of querying
`ghc-pkg` to obtain it.

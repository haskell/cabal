---
synopsis: Fix `cabal install` of a package from a tarball URI
packages: [cabal-install]
prs: 12499
issues: 7360
---

Brings back installing from a package referenced by tarball URI.  The fix was to
select the package by name only once the tarball has been read, the same way a
package named on the command line is.

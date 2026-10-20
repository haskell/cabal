---
synopsis: Let `--allow-boot-library-installs` rebuild a wired-in package the project provides
packages: [cabal-install]
---

With a compiler that reports wired-in unit ids (GHC 9.14 and later), `ghc` and
`ghc-internal` were pinned to the compiler's own installed copies even when the
project itself provides them as local packages, which made GHC's own source
tree unsolvable for cabal. With `--allow-boot-library-installs` such a local
package is now built from the project's source; wired-in packages that are
merely depended upon stay pinned.

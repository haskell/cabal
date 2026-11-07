---
synopsis: Avoid reconfiguring packages for Haddock output settings
packages: [cabal-install]
prs: [12491]
---

Changes to Haddock invocation settings, such as CSS, Quickjump, and contents or
index links, no longer invalidate the package configuration cache. Switching
between `cabal build` and `cabal haddock-project` can now reuse the existing
package configuration when only these settings differ.

Compiler and program settings still invalidate the cache when reconfiguration
is needed.

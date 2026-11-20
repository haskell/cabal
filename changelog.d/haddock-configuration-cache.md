---
synopsis: Avoid reconfiguring packages for Haddock output settings
packages: [cabal-install]
prs: [12491]
---

Changes to Haddock invocation settings, such as CSS, Quickjump, and contents or
index links, no longer invalidate the package configuration cache. Switching
between `cabal build` and `cabal haddock-project` can now reuse the existing
package configuration, avoiding unnecessary `Setup configure` and recompilation
cycles for local packages. Haddock is run again when these settings change.

The documentation monitor compares the effective HTML default and is checked
only for components that will actually run Haddock. This avoids repeatedly
scheduling sublibraries that normal builds do not document. Ephemeral output
overrides, such as the index URLs used by `haddock-project`, do not replace the
documentation settings remembered for normal builds.

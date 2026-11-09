---
synopsis: Top-level extra-prog-path only applies to local packages
packages: [cabal-install]
---

A top-level `extra-prog-path` in a project file now properly applies to local
packages only, as documented.

To recover the old behaviour, include the `extra-prog-path` inside a `package *`
stanza instead.

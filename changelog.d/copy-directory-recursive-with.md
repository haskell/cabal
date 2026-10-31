---
synopsis: Add `copyDirectoryRecursiveWith`
packages: [Cabal]
prs: [12491]
---

The new `copyDirectoryRecursiveWith` helper in `Distribution.Simple.Utils`
generalises `copyDirectoryRecursive`, allowing callers to choose the file
copying operation.

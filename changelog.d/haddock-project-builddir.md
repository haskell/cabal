---
synopsis: Honor the build directory when collecting project documentation
packages: [cabal-install]
prs: [12491]
---

`cabal haddock-project --builddir=DIR` now uses the requested directory for
planning, building, and collecting documentation. Previously, the collection
step searched the default build directory, which could omit project packages
from the generated contents page and leave an empty index.

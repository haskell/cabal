---
synopsis: Use unicode characters in the cabal-testsuite test directory
packages: [cabal-install, Cabal]
prs: 12619
issues: [12626, 12580]
---

Generate unicode characters in the temporary test directory to test cabal's
ability to handle them on user systems.

Fixes multiple sources of bugs for handling paths containing UTF-8 encoded unicode characters.

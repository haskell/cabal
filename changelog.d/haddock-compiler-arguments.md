---
synopsis: Preserve configured compiler arguments when generating documentation
packages: [Cabal]
prs: [12491]
---

Haddock now receives the configured GHC program arguments, including compiler
plugins and their package databases, in the same default/component/override
order as normal builds. GHC options passed directly to Setup's Haddock command
are included once.

Shared-only documentation builds also retain the component's ordinary GHC
options, and vanilla documentation builds include its static GHC options.
These changes let Haddock reuse compatible compilation artifacts instead of
reprocessing modules because compiler options or plugins were missing.

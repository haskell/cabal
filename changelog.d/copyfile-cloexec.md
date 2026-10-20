---
synopsis: Fix intermittent failure to run a just-installed build tool
packages: [Cabal]
---

During a parallel build, a process started while `copyFile` was running could
keep the new file open for writing, so running it failed with `ETXTBSY` until
that process exited. This showed up as `The program 'alex' version >=3.2.6 is
required but the version of ... could not be determined`.

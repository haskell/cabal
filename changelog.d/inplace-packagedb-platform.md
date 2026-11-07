---
synopsis: Key the inplace package database by the platform the compiler targets
packages: [cabal-install]
prs: 12356
---

The database that inplace packages are registered in now names the platform
the compiler targets, ahead of the compiler itself:

```
dist-newstyle/packagedb/<platform>/<compiler-id>
```

This is the segment order the build directory has always used
(`dist-newstyle/build/<platform>/<compiler-id>/...`).

Inplace unit ids do not mention the compiler, so two compilers that agree on
their id but target different platforms would register their copies of a
local package over one another in a database keyed by the compiler alone.

An existing inplace package database under the old path is ignored; local
packages are re-registered under the new one on the next build.

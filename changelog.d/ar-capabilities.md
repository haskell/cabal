---
synopsis: Check the archiver's capabilities against the archiver in use
packages: [Cabal]
prs: 0
---

Whether `ar` accepts the `-L` modifier and `@file` response files was taken
from the compiler's settings, which describe the `ar` GHC was configured with.
The `ar` Cabal runs can be another one, notably under cross-compilation, where
the compiler's is the target's (`llvm-ar`, `emar`) while the configured one is
the build machine's: GNU `ar` then rejects `-L`, and Apple's `ar` takes
`@file` as a member name. Each claimed capability is now confirmed against the
program's `--version` banner, falling back to the conservative invocation.

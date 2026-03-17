---
synopsis: Update to semaphore-compat 2.0.0 (`-jsem` protocol v2)
packages: [cabal-install]
prs: 11628
issues: 9993
---

On Linux and other POSIX platforms, cabal-install's `--semaphore`
jobserver now speaks v2 of the semaphore-compat protocol, which uses
Unix domain sockets in place of POSIX named semaphores. The v1
implementation used POSIX named semaphores via `sem_open(3)`, whose
ABI varies between C standard libraries; a `cabal-install` and `ghc`
built against different libc could not share a semaphore, breaking
`-jsem` whenever the toolchain wasn't homogeneous (see
[cabal #9993](https://github.com/haskell/cabal/issues/9993) and
[GHC #25087](https://gitlab.haskell.org/ghc/ghc/-/issues/25087)).
The v2 wire format is independent of libc. Windows is unaffected
and continues to use the v1 protocol (Win32 named semaphores).

cabal-install now inspects the selected GHC's `"Semaphore version"`
entry in `ghc --info` (via the new `jsemVersion` field on the `Cabal`
library's `Compiler` type) to detect a protocol mismatch ahead of
time. If GHC's reported version is incompatible with the version
cabal-install supports, cabal-install emits a warning of the form

    Semaphore version mismatch (cabal-install uses vN, but the
    selected GHC reports vM); not using -jsem, GHC will be invoked
    without semaphore-based parallelism.

and falls back to its in-process `NumJobs` coordinator instead of
passing `-jsem` to GHC. The build still succeeds, but loses the
cross-process module-level parallelism. Upgrading GHC to one that
supports protocol v2 restores full parallelism.

See also:

- the [GHC proposal amendment](https://github.com/ghc-proposals/ghc-proposals/pull/673)
- the [GHC patch](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/15729)
- the [semaphore-compat library MR](https://gitlab.haskell.org/ghc/semaphore-compat/-/merge_requests/8)

---
synopsis: "Program search paths: build tools, extra-prog-path, PATH"
packages: [Cabal, cabal-install]
prs: 12556
issues: [9756, 12544]
---

The program search path used to find programs is now consistently structured
to avoid duplicates and enforce the following ordering:

  (B) Programs specified in `build-tool-depends`
  (E) Programs found from `--extra-prog-path`
  (P) Fallback search on `PATH`.

cabal-install now passes the directories of `build-tool-depends` executables to
`Setup` as `--extra-prog-path` instead of adding them to the `PATH` of the
`Setup` process (except for `Setup` scripts built against Cabal < 3.19,
which need them in `PATH` to find them first).

When running a `Setup` script directly, and in v1 commands, the
`--extra-prog-path` directories now take precedence over `PATH`.

Custom Setup scripts will now need to use Cabal's program database to run
external programs that Cabal finds, as they are no longer added to `PATH`.
Alternatively, they can add the `--extra-prog-path` directories to `PATH`
before invoking.

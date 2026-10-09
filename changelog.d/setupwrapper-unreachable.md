---
synopsis: Make impossible cases in SetupWrapper unrepresentable
packages: [cabal-install]
prs:
---

`SetupMethod` is now indexed by the `SetupWrapperSpec` of the call and the
in-library method carries its arguments, so selecting the in-library method for
an external-only call, or for a post-configure phase without a
`LocalBuildInfo`, no longer type checks. The corresponding internal `error`
calls are gone.

`build-type: Make` packages (still parsed for `cabal-version` < 3.18) and
`build-type: Hooks` with a Cabal library older than 3.13 now fail with a proper
error message instead of an internal error.

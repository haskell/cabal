---
synopsis: Pass GNU triples to configure scripts when cross-compiling
packages: [Cabal]
prs: 0
---

When the host platform differs from the build platform, a package's
`configure` script now receives `--build=` and `--host=` as GNU configuration
names (`x86_64-unknown-linux`, `wasm32-unknown-wasi`,
`javascript-unknown-ghcjs`, ...) instead of Cabal's own platform spelling
(`x86_64-linux`, `wasm32-wasi`, `javascript-ghcjs`), which autoconf's
`config.sub` rejects for most cross targets.

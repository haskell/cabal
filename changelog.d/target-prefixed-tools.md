---
synopsis: Find a cross-compiler's target-prefixed companion tools
packages: [Cabal]
prs: 0
---

When the compiler is a target-prefixed ghc, such as `wasm32-unknown-wasi-ghc`
or `javascript-unknown-ghcjs-ghc`, its companion tools (`ghc-pkg`, `hsc2hs`,
`haddock`, ...) are looked for under the same prefix next to it first. Before,
only the bare or version-suffixed names were tried, so the bare `ghc-pkg` next
to the compiler, or the one on the `PATH`, was used instead: a different
compiler's tool.

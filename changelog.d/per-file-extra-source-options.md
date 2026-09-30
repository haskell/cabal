---
synopsis: Per-file options for extra source files
packages: [Cabal, Cabal-syntax, Cabal-hooks]
prs: 12288
significance: significant
---

The extra-source fields (`c-sources`, `cxx-sources`, `asm-sources`,
`cmm-sources`, `js-sources`) now accept per-file options, written in
parentheses after each file:

```cabal
c-sources: foo.c (-DFOO -O2) bar.c
cmm-sources: rts.cmm (-mavx2)
```

The options are passed to the compiler only when compiling that particular
file. This requires `cabal-version: 3.20` or later; using the syntax with an
earlier `cabal-version` is a parse error, so nothing changes for existing
packages.

An option that contains whitespace, a `)` or a `"` has to be written as a
Haskell string literal, e.g. `c-sources: greet.c ("-DGREETING=\"hi there\"")`.

The five fields now hold `ExtraSource` values rather than plain paths, so code
that reads or writes `cSources` and friends has to change. Use
`extraSourceFromPath` to make an `ExtraSource` with no options.
`Distribution.Simple.SetupHooks` exports both, so a `Hooks` package does not
need a direct dependency on `Cabal-syntax`.

Options are accepted on all five kinds. For `c-sources`, `cxx-sources` and
`asm-sources` they are passed as compiler options (`-optc`/`-optcxx`/`-opta`).
GHC compiles C-- sources itself, so options on `cmm-sources` are passed to GHC,
next to the ones from the `cmm-options` field. JavaScript sources are only
preprocessed, so options on `js-sources` are passed to the JavaScript
preprocessor (`-optJSP`), which requires GHC 9.12 or later; with an older GHC
they are ignored and Cabal warns. The legacy GHCJS compiler does not
preprocess JavaScript sources at all — it only passes them to `-link-js-lib`
at link time — so options on `js-sources` are ignored there too, again with a
warning.

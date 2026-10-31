---
synopsis: Add `--PROG-prepend-option` to `Setup build`, `Setup haddock` and `Setup repl`
packages: [Cabal]
prs: [12667]
---

`Setup build`, `Setup haddock` and `Setup repl` accept `--PROG-prepend-option`
to supply invocation-only defaults for a program. Unlike `--PROG-option`
arguments, which follow the configured program arguments, these precede them,
so that explicitly configured options take precedence.

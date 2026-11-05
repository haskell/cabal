---
synopsis: Stop doubling the leading slash of the `configure` script path on POSIX
packages: [Cabal]
prs: 0000
---

`build-type: Configure` packages ran `./configure` as `//path/to/configure`.
Autoconf scripts derive `$srcdir` from that path, so `-I` flags built from it
started with `//`. A tool that compares such paths as strings could then fail
to recognise them: nixpkgs' cc-wrapper drops `-I` flags outside its allowed
prefixes.

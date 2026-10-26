---
synopsis: Add --with-build-PROG and --build-PROG-options for the build stage
packages: [cabal-install]
prs: 12356
---

When cross-compiling, the programs of the *build* stage can now be configured
separately from those of the *host* stage. `--with-PROG`, `--PROG-options`,
`program-locations` and `program-options` apply to the host stage only, like
`--with-compiler`; the build stage has its own settings and does not inherit
the host stage's.

- New command-line flags `--with-build-PROG`, `--build-PROG-option` and
  `--build-PROG-options`.
- New `cabal.project` stanzas `build-program-locations` and
  `build-program-options`, and `package` stanza fields `build-PROG-location`
  and `build-PROG-options`.

Without cross-compilation there is no build stage: these settings have no
effect and `cabal` warns that they are ignored.

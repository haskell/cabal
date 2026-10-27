---
synopsis: Add --with-build-PROG and --build-PROG-options for the build machine
packages: [cabal-install]
prs: 12356
---

When cross-compiling, the programs of the *build* machine can now be configured
separately from those of the *host* machine. `--with-PROG`, `--PROG-options`,
`program-locations` and `program-options` apply to the host machine only, like
`--with-compiler`; the build machine has its own settings and does not inherit
the host machine's.

- New command-line flags `--with-build-PROG`, `--build-PROG-option` and
  `--build-PROG-options`.
- New `cabal.project` stanzas `build-program-locations` and
  `build-program-options`, and `package` stanza fields `build-PROG-location`
  and `build-PROG-options`.

Without cross-compilation there is no build machine: these settings have no
effect and `cabal` warns that they are ignored.

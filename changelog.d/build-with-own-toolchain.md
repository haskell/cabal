---
synopsis: Build-machine packages are built, registered and validated with the build toolchain
packages: [cabal-install, cabal-install-solver]
prs: 12356
---

When cross-compiling (`--with-build-compiler`), packages that belong to the
build machine — build tools and custom `Setup.hs` dependencies built from
source — are now configured, built and registered with the build compiler,
into that compiler's package databases, and the solver validates them
against that compiler's extensions, languages and pkg-config packages.
Previously the in-process build path and the package-database plumbing used
the host toolchain for every package.

The project's package-db settings stay with the host compiler. A package
database is only readable by the compiler that wrote it, so `package-dbs:`
and `--package-db` apply to the host machine alone; the build machine uses its
own compiler's global database and its own store. There is currently no way
to name additional databases for the build machine.

No change for ordinary (non-cross) builds, where there is no separate build
machine and the settings apply everywhere as before.

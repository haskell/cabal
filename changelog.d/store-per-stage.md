---
synopsis: Lay out the store by machine and toolchain
packages: [cabal-install]
prs:
significance: significant
---

The store is now keyed by the machine a package is built for and that
machine's toolchain:

```
store/<machine>/<platform>/<unit-id>
store/<machine>/<platform>/package.conf.d
```

replacing `store/<compiler-id>-<abi>/<unit-id>`. The old key records which
compiler built a package but not what that compiler targets, so two
toolchains that differ only in their target platform share one store
directory and one package database. Under cross-compilation that is the
ordinary case, since a cross compiler usually carries the same version as
the build compiler. The artifacts themselves stay separate -- cabal hashes
both the compiler id and the platform into the unit id -- but the database
is left holding entries for two platforms, written and read by two
different `ghc-pkg`s.

Two spellings change with it, so that the paths can be consumed by another
build system reading the store directly, as GHC's own does:

- the package database is named `package.conf.d`, as GHC spells it, rather
  than `package.db`;
- the platform segment is the triple the compiler reports as its target
  platform (`x86_64-unknown-linux`) rather than cabal's own spelling
  (`x86_64-linux`).

Call sites that hold an elaborated package now use that package's own machine
and toolchain instead of the project's host compiler. This fixes two places
that were pinned to the host machine: the store entry and log file of a
build-machine package, and plan improvement, which previously matched one
machine's store entries against packages of either machine.

Consequences for existing installations:

- Anything that reads paths under `~/.local/state/cabal/store` directly
  (including `cabal path --store-dir` consumers) will need updating.
- Existing store entries under the old `store/<compiler-id>-<abi>/` layout
  are not migrated or read; packages are rebuilt into the new layout on first
  use.
- The compiler id and ABI are no longer part of the store path, so two
  different compilers for the same platform share a store directory and
  package database. This is only sound with a project-local store, which a
  later change in this series introduces; until then this is a regression
  for the shared global store.

---
synopsis: Key the store and the inplace package database by the platform the compiler targets
packages: [cabal-install]
prs: 12356
significance: significant
---

Store locations now name the platform the compiler targets, ahead of the
compiler itself:

```
store/<platform>/<compiler-id>-<abi>/<unit-id>
store/<platform>/<compiler-id>-<abi>/package.db
```

The old key was the compiler's id and ABI tag alone, which records which
compiler built a package but not what that compiler produces. Two compilers
that agree on both — a host compiler targeting one architecture and a build
compiler targeting another — were filed in one directory, under one package
database. Their `unit-id`s differ, since cabal hashes the platform along with
the compiler id, so the builds did not overwrite each other; but one
directory ended up holding binaries for two architectures, and one package
database entries for both, to be read back by two different `ghc-pkg`s.

The segment order matches the build directory, which has always been laid out
this way (`dist-newstyle/build/<platform>/<compiler-id>/...`).

The database that inplace packages are registered in is keyed the same way,
for a second reason: inplace unit ids do not mention the compiler at all, so
two compilers that agree on their id would register the two copies of a local
package over one another.

```
dist-newstyle/packagedb/<platform>/<compiler-id>
```

Consequences for existing installations:

- Existing store entries under the old layout are neither migrated nor read.
  They are left in place and ignored; packages are rebuilt into the new
  layout on first use. The same goes for the inplace package database.
- Anything reading paths under the store directly, including consumers of
  `cabal path --store-dir`, needs the extra path segment.

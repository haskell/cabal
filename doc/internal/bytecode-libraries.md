# Bytecode Libraries in `cabal-install`

This note explains how `--enable-library-bytecode` is implemented across
`cabal-install` and the `Cabal` library.

The short version is:

1. `cabal-install` parses and stores a per-package `library-bytecode` flag.
2. The `Cabal` library turns that boolean into GHC flags which produce `.gbc`
   object files and then links them into a `.bytecodelib`.
3. Installation copies the resulting `.bytecodelib` next to the other library
   artifacts.

Most of the implementation lives in `Cabal`; `cabal-install` mostly propagates
configuration.

## User-facing entry points

The feature is exposed in the same places as other per-package build options.

- `cabal.project` parsing accepts `library-bytecode` in
  `cabal-install/src/Distribution/Client/ProjectConfig/FieldGrammar.hs`.
- The corresponding per-package field is
  `packageConfigBytecodeLib` in
  `cabal-install/src/Distribution/Client/ProjectConfig/Types.hs`.
- The `Setup configure` flag is `--enable-library-bytecode` /
  `--disable-library-bytecode`, wired through
  `Cabal/src/Distribution/Simple/Setup/Config.hs`.
- Installation also now has a dedicated `--bytecodelibdir` flag, wired through
  the normal `InstallDirs` machinery in
  `Cabal/src/Distribution/Simple/InstallDirs.hs` and
  `Cabal/src/Distribution/Simple/Setup/Config.hs`.

At this layer there is nothing specific to artifact production yet; the option
is still just a boolean.

## What `cabal-install` does

`cabal-install` adds the project setting to the elaborated build plan in
`cabal-install/src/Distribution/Client/ProjectPlanning.hs`.

When each package gets its `BuildOptions`, `withBytecodeLib` is set from
`packageConfigBytecodeLib`:

- `withBytecodeLib = perPkgOptionFlag pkgid False packageConfigBytecodeLib`

After that point, `cabal-install` is mostly done. The important consequence is
that bytecode libraries become part of the per-package configuration and
therefore:

- affect how the package is built;
- affect the package hash via
  `cabal-install/src/Distribution/Client/PackageHash.hs`; and
- participate in plan/configure state like other build-style toggles.

## What `Cabal` stores

The boolean lands in `BuildOptions` / `LocalBuildInfo` as `withBytecodeLib`.
The relevant definitions are in:

- `Cabal/src/Distribution/Types/LocalBuildConfig.hs`
- `Cabal/src/Distribution/Types/LocalBuildInfo.hs`

This is the bit of state the build pipeline consults later.

## Compiler support check

Bytecode artifacts are currently gated on compiler support in
`Cabal/src/Distribution/Simple/Compiler.hs`.

Today the predicate is intentionally simple:

- only GHC is supported; and
- support starts at GHC `9.15.0`.

During `configure`, `Cabal/src/Distribution/Simple/Configure.hs` checks this
predicate. If the compiler does not support bytecode artifacts, Cabal warns and
silently clears `withBytecodeLib`.

That means `cabal-install` can always plan with the flag present, while the
final decision about whether the compiler can honor it remains in the lower
level build system.

This mirrors how other features are disabled if the compiler does not support
building that configuration.

## Compiler bytecode artifacts and libraries

The core compilation logic is in
`Cabal/src/Distribution/Simple/GHC/Build/Modules.hs`.

Once `withBytecodeLib lbi` is true, Cabal asks GHC to emit bytecode objects in
addition to normal object code.

The important detail is that bytecode objects are never their own independent
build way. They are always produced alongside one existing native library way:

- if `DynWay` is being built, bytecode is attached to the dynamic compilation;
- otherwise bytecode is attached to the static compilation.
- Note that "dynamic-too" means that often dynamic objects are produced during "static way" compilation.

So the choice of "which way owns the `.gbc` files" is deterministic:
dynamic when available, otherwise use static.

An important constraint is enforced here as well: Cabal refuses to build a
package with only bytecode library output. In
`Distribution.Simple.GHC.Build.Modules`, if bytecode is enabled but neither the
static nor dynamic library way is being built, `Cabal` throws an error.

That guard exists because bytecode is currently defined as a companion artifact,
not a standalone way. The rest of the ecosystem still assumes there is a normal
library artifact alongside the bytecode one.

## How the `.bytecodelib` is linked

After module compilation, library linking happens in
`Cabal/src/Distribution/Simple/GHC/Build/Link.hs`.

This stage does two separate things:

- it gathers the generated `.gbc` files; and
- it invokes GHC again with `-bytecodelib` to combine them into a single
  library artifact.

The linker arguments are built by `ghcBytecodeLinkArgs`, which sets:

- `ghcOptBytecodeLib = True`
- the output path to `mkBytecodeLibName ...`
- the input files to the bytecode object files collected for the chosen way

`mkBytecodeLibName` is defined in
`Cabal/src/Distribution/Simple/BuildPaths.hs` and currently produces:

- `libHS<unit-id>.bytecodelib`

The same companion-way rule from module compilation is repeated here:

- in the `DynWay` branch, Cabal links the `.bytecodelib` from the `DynWay`
  bytecode objects;
- in the `StaticWay` branch, Cabal only links the `.bytecodelib` from
  `StaticWay` when `DynWay` was not requested.

So if both static and dynamic libraries are built, there is still only one
bytecode library, and it is linked from the bytecode objects generated by the
dynamic compilation path. Static is only the fallback owner when there is no
separate dynamic way/if -dynamic-too is used.

## How installation works

Library installation lives in `Cabal/src/Distribution/Simple/GHC.hs`.

If the component has code, is a library, and `withBytecodeLib` is enabled, the
installer copies the `.bytecodelib` from the build directory into the
configured bytecode-library directory.

This directory now comes from `InstallDirs` as `bytecodelibdir`. By default it
is:

- `$libdir/$libsubdir`

but it can be overridden explicitly with `--bytecodelibdir`.

## Registration metadata

`InstalledPackageInfo` already has a `libraryBytecodeDirs` field in
`Cabal-syntax`, and `Cabal/src/Distribution/Simple/Register.hs` now populates
it from `bytecodelibdir`.

## Tests to read first

The most useful tests for understanding intended behavior are:

- `cabal-testsuite/PackageTests/LibraryBytecode/cabal.test.hs`
  checks that building with `--enable-library-bytecode` produces a
  `.bytecodelib`.
- `cabal-testsuite/PackageTests/LibraryBytecodeCSources/cabal.test.hs`
  checks the same path for a library with C sources.
- `cabal-testsuite/PackageTests/LibraryBytecodeUnsupported/cabal.test.hs`
  checks the unsupported-compiler warning path.
- `cabal-testsuite/PackageTests/LibraryBytecodeStandalone/cabal.test.hs`
  checks the rejection of bytecode-only library builds.
- `cabal-testsuite/PackageTests/LibraryBytecodeDir/setup.test.hs`
  checks that `--bytecodelibdir` controls both the install location and the
  registered `bytecode-library-dirs` field.
- `cabal-testsuite/PackageTests/LibraryBytecodeInstall/cabal.test.hs`
  checks that installed bytecode libraries are usable from `cabal repl` with
  `-fprefer-byte-code`.

## Mental model

If you want a compact mental model of the implementation, it is this:

- `cabal-install` decides *whether* a package wants bytecode libraries.
- `Cabal` decides *whether the compiler supports them*.
- the GHC build pipeline emits `.gbc` side artifacts while compiling modules.
- the GHC link step packs those `.gbc` files into a `.bytecodelib`.
- install copies the resulting file into `bytecodelibdir`.
- registration records that same directory in `bytecode-library-dirs`.

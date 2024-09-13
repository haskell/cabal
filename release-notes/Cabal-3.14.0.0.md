### Significant changes

- Neutral field to add files to sdist [#8817](https://github.com/haskell/cabal/issues/8817) [#10107](https://github.com/haskell/cabal/pull/10107)

  Adds the `extra-files` field to the cabal file specification. This is like
  the other `extra-*` fields in that it is copied with the `sdist` command,
  except there are no other semantics. Compare to:

  * `extra-source-files`: Tracked by `cabal build`.

  * `extra-doc-files`: Copied by Haddock to the html directory.

### Other changes

- Include package version when passing `--promised-dependency` flag [#10166](https://github.com/haskell/cabal/issues/10166) [#10248](https://github.com/haskell/cabal/pull/10248)

  The `--promised-dependency` flag now expects an argument in the format

  ```
  NAME-VER[:COMPONENT_NAME]=CID
  ```

  rather than

  ```
  NAME[:COMPONENT_NAME]=CID
  ```

- Add support for building profiled dynamic way [#4816](https://github.com/haskell/cabal/issues/4816) [#9900](https://github.com/haskell/cabal/pull/9900)

  Add support for profiled dynamic way

  New options for `cabal.project` and `./Setup` interface:

  * `profiling-shared`: Enable building profiling dynamic way
  * Passing `--enable-profiling` and `--enable-executable-dynamic` builds
    profiled dynamic executables.

  Support for using `profiling-shared` is guarded behind a constraint
  which ensures you are using `Cabal >= 3.13`.

  In the cabal file:

  * `ghc-prof-shared-options`, for passing options when building in
    profiling dynamic way

- Working directory support for `Cabal` [#9702](https://github.com/haskell/cabal/issues/9702) [#9718](https://github.com/haskell/cabal/pull/9718)

  The `Cabal` library is now able to handle a passed-in working directory, instead
  of always relying on the current working directory of the parent process.

  In order to achieve this, the `SymbolicPath` abstraction was fleshed out, and
  all fields of `PackageDescription` that, if relative, should be interpreted
  with respect to e.g. the package root, use `SymbolicPath` instead of `FilePath`.

  This means that many library functions in `Cabal` take an extra argument of type
  `Maybe (SymbolicPath CWD (Dir "Package"))`, which is an optional (relative or
  absolute) path to the package root (if relative, relative to the current working
  directory). In addition, many functions that used to manipulate `FilePath`s now
  manipulate `SymbolicPath`s, require explicit conversion using e.g. `getSymbolicPath`.

  To illustrate with file searching, the `Cabal` library defines:

  ```haskell
  findFileCwd
    :: forall dir1 dir2 file
     . Verbosity
    -> Maybe (SymbolicPath CWD (Dir dir1))

    -> [SymbolicPath dir1 (Dir dir2)]

    -> RelativePath dir2 File

    -> IO (SymbolicPath dir1 File)
  ```

  See Note [Symbolic paths] in `Distribution.Utils.Path` for further information
  on the design of this API.

- Add `MultilineStrings` extension (GHC proposal #637) [#10245](https://github.com/haskell/cabal/pull/10245)

- Add `NamedDefaults` extension (GHC proposal #409) [#9740](https://github.com/haskell/cabal/pull/9740)

- Add `OrPatterns` extension (GHC proposal #958) [#10339](https://github.com/haskell/cabal/pull/10339)


### Other changes

- Add flag `--ignore-build-tools` [#10128](https://github.com/haskell/cabal/pull/10128)

  - Adds flag `--ignore-build-tools` which allows a user to ignore the tool
    dependencies declared in `build-tool-depends`. For general use, this flag
    should never be needed, but it may be useful for packagers.

- Do not try to build dynamic executables on Windows [#10217](https://github.com/haskell/cabal/pull/10217)

  - Cabal will now exit with a descriptive error message instead of attempting to
    build a dynamic executable on Windows.

- Always pass `ghc-options` to GHC [#8717](https://github.com/haskell/cabal/pull/8717)

  Previously, options set in the package field `ghc-options` would not be passed
  to GHC during the link phase for shared objects (where multiple `.o` or
  `.dyn_o` files are merged into a single object file). This made it impossible
  to use `ghc-options` to use a different linker by setting (for example)
  `ghc-options: -optl-fuse-ld=mold -optlm-fuse-ld=mold`; the options would be
  dropped in the link phase, falling back to the default linker.

  It was possible to work around this by duplicating the `ghc-options` to
  `ghc-shared-options`, which _are_ passed in the shared link phase, but that had
  the undocumented and unfortunate side-effect of disabling the GHC
  `-dynamic-too` flag, effectively doubling compilation times when
  `ghc-shared-options` are set.

  Now, `ghc-options` are combined with `ghc-shared-options` (to accurately
  reflect the documentation on this feature) and the fact that
  `ghc-shared-options` disables `-dynamic-too` is documented.

- Introduce `SetupHooks` [#9551](https://github.com/haskell/cabal/pull/9551)

  Introduction of a new build type: `Hooks`.
  This build type, intended to eventually replace the `Custom` build type, integrates
  better with the rest of the ecosystem (`cabal-install`, Haskell Language Server).

  The motivation and full design of this new build-type are specified in the
  Haskell Foundation Tech Proposal
  [Replacing the Cabal Custom build-type](https://github.com/haskellfoundation/tech-proposals/pull/60).

  Package authors willing to use this feature should declare `cabal-version: 3.14` and `build-type: Hooks`
  in their `.cabal` file, declare a `custom-setup` stanza with a dependency on the
  `Cabal-hooks` package, and define a module `SetupHooks` that exports a value
  `setupHooks :: SetupHooks`, using the API exported by `Distribution.Simple.SetupHooks`
  from the `Cabal-hooks` package. Refer to the Haddock documentation of
  `Distribution.Simple.SetupHooks` for example usage.

- Redefine `build-type: Configure` in terms of `Hooks` [#9969](https://github.com/haskell/cabal/pull/9969)

  The `build-type: Configure` is now implemented in terms of `build-type: Hooks`
  rather than in terms of `build-type: Custom`. This moves the `Configure`
  build-type away from the `Custom` issues. Eventually, `build-type: Hooks` will
  no longer imply packages are built in legacy-fallback mode. When that
  happens, `Configure` will also stop implying `legacy-fallback`.

  The observable aspect of this change is `runConfigureScript` now having a
  different type, and `autoconfSetupHooks` being exposed by `Distribution.Simple`.
  The former is motivated by internal implementation details, while the latter
  provides the `SetupHooks` value for the `Configure` build type, which can be
  consumed by other `Hooks` clients (e.g. eventually HLS).

- Cabal can issue a number of error messages referencing "Setup configure",
  but it simply references "configure" which could mean any of three
  things (Setup configure, the package's "configure" script, or "cabal
  configure"). This has recently caught out even Cabal devs. Clarify these
  messages. [#9476](https://github.com/haskell/cabal/pull/9476)

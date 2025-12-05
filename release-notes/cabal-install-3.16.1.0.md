cabal-install and cabal-install-solver 3.16.1.0 changelog and release notes
---

### Significant changes

- Don't pass the executable name to external commands [#10275](https://github.com/haskell/cabal/issues/10275) [#11232](https://github.com/haskell/cabal/pull/11232)

  Previously the executable name of the external command was passed to external commands as the first argument.

  This behaviour was adapted from cargo which does this because of reasons that are internal to rust that do not affect GHC Haskell, and are even orthogonal to patterns that see common use in Haskell.

  Additionally, it complicates the 'simple' case which is what we should optimize for when building such a feature - with this change, for any executable `cabal-foo` in your search-path, `cabal foo` will be a valid invocation of that command.

  The previous use case (one executable that serves multiple external subcommands) is still possible by the following means:

  - using a wrapper around the executable
  - using a symlink and check argv\[0\] in the executable

  Additionally, the variable `$CABAL` that was set by `cabal-install` was renamed to `CABAL_EXTERNAL_CABAL_PATH`. This has two reasons:
  1. it makes migration easier for users of the external command feature that were previously expecting the name of the executable to appear in `argv[1]`

  2. it does not unnecessarily pollute the environment variable namespace as it turns out some other tools have been and are already using this name, historically

### Other changes

- Add compiler ABI tag and store path to `cabal path` output [#10165](https://github.com/haskell/cabal/issues/10165) [#11266](https://github.com/haskell/cabal/pull/11266)

  This adds the GHC Project Unit ID (ABI tag) and store path to the cabal path output as "compiler-abi-tag" and "compiler-store-path". These have been used in cabal-install since 3.12.1.0.

- Better error messages when repl command is missing a target [#10527](https://github.com/haskell/cabal/issues/10527) [#10684](https://github.com/haskell/cabal/pull/10684)

  With a project, the REPL command requires a target. If one is not given then a
  message is shown explaining this and naming the project if the `--project-file`
  option was given (but not when the default 'cabal.project' project name is used
  implicitly).  We're not yet able to list project targets so in the meantime, the
  messages lists the packages of the project.

  Before the fix the message mentioned a `fake-package-0`. This was confusing. It is no longer mentioned.

- Fix regression when -b option is used in a non-project context [#11107](https://github.com/haskell/cabal/issues/11107) [#11237](https://github.com/haskell/cabal/pull/11237)

  The `repl -b` option can be used to create a repl which contains certain specific
  packages.

  `cabal repl -b vector`, starts a GHCi session with the `vector` package available.

  The flag was broken in the 3.16.0.0 release, and now fixed.


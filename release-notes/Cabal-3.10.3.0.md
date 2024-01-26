Cabal and Cabal-syntax 3.10.3.0 changelog and release notes
---

## Release 3.10.3.0 is strictly a bug-fix release, with the fixes listed below

- PkgConfig environment variables [#9134](https://github.com/haskell/cabal/pull/9134)
  - `cabal` invokes `pkg-config` with `PKG_CONFIG_ALLOW_SYSTEM_CFLAGS` and `PKG_CONFIG_ALLOW_SYSTEM_LIBS` set

- Support text-2.1 in Cabal and Cabal-syntax [#9242](https://github.com/haskell/cabal/pull/9242)

- Fix extra-prog-path propagation [#7649](https://github.com/haskell/cabal/issues/7649) [#9519](https://github.com/haskell/cabal/issues/9519) [#9527](https://github.com/haskell/cabal/pull/9527)
  - extra-prog-paths are now propagated to all commands. This in particular helps
    when running a MinGW cabal in the PowerShell, where the MSYS2 paths are
    usually not available in the PowerShell PATH. GHCup already sets them up for
    us but they were sometimes lost on the way.

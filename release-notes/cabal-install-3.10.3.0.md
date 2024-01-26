cabal-install and cabal-install-solver 3.10.3.0 changelog and release notes
---

## Release 3.10.3.0 is strictly a bug-fix release, with the fixes listed below

- PkgConfig individual calls [#9134](https://github.com/haskell/cabal/pull/9134)

  - `cabal` invokes `pkg-config` individually for each lib if querying for all doesn't return the expected result

- Use compiler flags for caching project config [#8819](https://github.com/haskell/cabal/pull/8819)

  This ensures that cached project configs with conditionals re-execute the conditional logic when the compiler changes.

- Make `check` recognise `TypeAbstractions` [#9496](https://github.com/haskell/cabal/issues/9496) [#9503](https://github.com/haskell/cabal/pull/9503)

  - `cabal check` will not complain about “Unknown extension” when
    finding `TypeAbstractions`.

- `check`: add PackageInfo warning [#9331](https://github.com/haskell/cabal/issues/9331) [#9525](https://github.com/haskell/cabal/pull/9525)

  `cabal check` will warn about PackageInfo_* modules and provide an upgrade path to 3.12.

- Fix extra-prog-path propagation [#7649](https://github.com/haskell/cabal/issues/7649) [#9519](https://github.com/haskell/cabal/issues/9519) [#9527](https://github.com/haskell/cabal/pull/9527)

  - extra-prog-paths are now propagated to all commands. This in particular helps
    when running a MinGW cabal in the PowerShell, where the MSYS2 paths are
    usually not available in the PowerShell PATH. GHCup already sets them up for
    us but they were sometimes lost on the way.

- fix pkgconfig-depends for pkgconf-1.9 [#8923](https://github.com/haskell/cabal/issues/8923) [#9391](https://github.com/haskell/cabal/pull/9391)

- Ignore invalid Unicode in pkg-config descriptions [#9608](https://github.com/haskell/cabal/issues/9608) [#9609](https://github.com/haskell/cabal/pull/9609)

  Previously, cabal-install would crash when `pkg-config --list-all` contained
  invalid Unicode. With this change, invalid unicode in package descriptions is
  ignored, and unparseable package names are considered nonexistent.

- Script cache dir is the base16 hash of the canonical path of the script. [#9459](https://github.com/haskell/cabal/pull/9459)

  Script cache dir is the base16 hash of the canonical path of the script.

- Fix run command environment [#8391](https://github.com/haskell/cabal/issues/8391) [#9341](https://github.com/haskell/cabal/pull/9341)

  - The Run command will now add binary paths of dependencies
    (build-tool-depends) to PATH, just like Exec and Test commands.

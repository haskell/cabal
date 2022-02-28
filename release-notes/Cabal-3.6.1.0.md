Cabal 3.6.1.0 Changelog
---

### Significant Changes

- Include cmm-sources when linking shared objects [#7182](https://github.com/haskell/cabal/issues/7182) [#7252](https://github.com/haskell/cabal/pull/7252)

  - Previously `cmm-sources` were not included in the final link when building a library as a shared object. Fix this.

- Prefer canonicalized path when guessing tools from GHC path [#7390](https://github.com/haskell/cabal/issues/7390) [#7392](https://github.com/haskell/cabal/pull/7392)

  Motivation

  Often times, the user facing `ghc` binary is
  symlinked by other forces, such as the package manager,
  tooling like ghcup etc. As such, the naming convention
  (version suffix in particular) may not align with the
  assumptions made in Cabal and it may find an incorrect ghc-pkg.

  See:
    - https://github.com/haskell/cabal/issues/7390
    - https://gitlab.haskell.org/ghc/ghc/-/issues/18807
    - https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/73

  Solution

  Guessing the ghc-pkg path is already a hack and will be solved
  more appropriately in the future, see
    - https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4214
    - https://gitlab.haskell.org/ghc/ghc/-/snippets/2710
  These patches will solve the issue for future GHC versions.

  As such, this patch provides a workaround for
  older, already existing GHC versions by first always
  following the symbolic link of the ghc binary (if it is one)
  and prefering its target directory as the guess lookup
  location.

  Rationale

  The canonicalized path of the ghc binary usually points to the
  bin/ directory unpacked from a bindist, which is less likely to be
  tampered with by distributions and tools. As such, prefering the
  canoncialized path should get us more robust results.

- Fix `cabal test --enable-library-coverage` for other-modules [#5213](https://github.com/haskell/cabal/issues/5213) [#7493](https://github.com/haskell/cabal/pull/7493)

  - Fix `cabal test --enable-library-coverage` for libraries with nonempty other-modules field.
  - Due to a hack, this breaks coverage whenever the used Haskell compiler is called 't' (for a non-hacky fix we should rework HPC directories, possibly enabling multilib in the process, see #6440 and #6397).

- Set PATH_SEPARATOR=";" when calling ./configure on Windows; this fix is necessary for autoconf >= 2.70 [#7494](https://github.com/haskell/cabal/issues/7494) [#7510](https://github.com/haskell/cabal/pull/7510)

- lazily decode cache files for checking invalidation [#7516](https://github.com/haskell/cabal/pull/7516) [#7466](https://github.com/haskell/cabal/issues/7466)
  - This yields a significant 15% speedup on rebuilding build plans for projects with lots of individual cabal packages.
  - It exports the [Tag data type](https://github.com/haskell/cabal/blob/899dd34bc48bbaa43da9a4b2fc354c24fd814d05/Cabal/src/Distribution/Utils/Structured.hs#L67), needed by `cabal-install >= 3.6.0.0`.

- defer build-tools-depends choices as well as setup choices [#7561](https://github.com/haskell/cabal/pull/7561) [#7472](https://github.com/haskell/cabal/issues/7472)
  - extends the existing solver pass that defers solving setup depends until top-level goals are solved to also defer build-tool-depends goals until top level goals are solved.
  - constraints at the top level (on versions, flags, stanzas, etc) are applied first and only then are attempts made to build the less-constrained qualified goals, significantly streamlining the production of valid build plans.

- Set -Wno-prepositive-qualified-module in Paths_*.hs [#7352](https://github.com/haskell/cabal/pull/7352)

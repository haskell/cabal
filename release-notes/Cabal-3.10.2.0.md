Cabal and Cabal-syntax 3.10.2.0 changelog and release notes
---

## Release 3.10.2.0 is strictly a bug-fix release, with the fixes listed below

- Shorten script-builds paths [#8841](https://github.com/haskell/cabal/issues/8841) [#8898](https://github.com/haskell/cabal/pull/8898)

  - Use Base64 hash truncated to 26 chars for script-build cache directories.
  - Use the cache directory as the dist directory.
  - Use script-<your-sanitized-script-name> as the component name instead of cabal-script-<...>.
  - Use cabal-script-<your-actual-script-name> for the executable name.
  - This change is incompatible with previous cabal versions in terms of cache location,
    you should manually remove your old caches once you no longer need them.

- Do not always pass --quickjump to haddock #9049 [#9060](https://github.com/haskell/cabal/issues/9060) [#9049](https://github.com/haskell/cabal/pull/9049)

  6d8adf13101 caused `cabal` to always pass the `--quickjump` flag to Haddock.
  Not only does this waste memory for a service that user hasn't asked for,
  but also leads to a failure with Haddocks shipped with GHC 9.0 and 9.2,
  which had a separate bug (fixed in later versions but not backported) when
  Haddock does not pass `--quickjump` recursively to the package dependencies.

- Add language extension ExtendedLiterals [#8992](https://github.com/haskell/cabal/pull/8992)

  - adds support for the ExtendedLiterals language extension (GHC proposal #451)

- Regenerate Lexer.hs to avoid out-of-bound array access due to a bug in Alex [#8892](https://github.com/haskell/cabal/issues/8892) [#8896](https://github.com/haskell/cabal/pull/8896)

  - Regenerate Cabal-syntax's Lexer.hs with Alex 3.2.7.3 which includes a fix for
    an out-of-bound array access (only noticeable with GHC's JavaScript backend).

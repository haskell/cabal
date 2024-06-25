Cabal and Cabal-syntax 3.12.1.0 changelog and release notes
---


### Significant changes

- Update GHC arguments normalization and GHC options rendering [#9729](https://github.com/haskell/cabal/issues/9729) [#10014](https://github.com/haskell/cabal/pull/10014)

  The flags `-fdiagnostics-as-json`, `-fprint-error-index-lists`, `-fbreak-points`, `-dipe-stats`, `-ffamily-application-cache`, `-fprint-redundant-promotion-ticks`, `-fshow-error-context` and `-funoptimized-core-for-interpreter` have been added to the flags that do not cause recompilation.

- Warn on missing `default-language` [#9620](https://github.com/haskell/cabal/issues/9620) [#9766](https://github.com/haskell/cabal/pull/9766)

  - To help the adoption of GHC language editions, `cabal check` will now
    warn about missing `default-language`.

- Add MHS ([MicroHS](https://github.com/augustss/MicroHs)) as a known Haskell compiler [#9878](https://github.com/haskell/cabal/pull/9878)

- Re-instate `initialBuildSteps` [#9856](https://github.com/haskell/cabal/issues/9856) [#9950](https://github.com/haskell/cabal/pull/9950)

  The `initialBuildSteps` function from `Distribution.Simple.Build`, which had
  been hastily removed, has been reinstated.

  It now comes with a deprecation warning: calling that function does not suffice
  to prepare the sources for a package, as there are other steps that one might
  also need to perform:

    - running pre-processors (such as alex/happy);
    - running pre-build hooks or custom logic
      (in build-type: Hooks or build-type: Custom or Configure).

  Consumers wanting to prepare the sources of a package, e.g. in order to launch a
  REPL session, are advised to run `setup repl --repl-multi-file=<fn>` instead.

- Label error messages with codes (following GHC, Stack)

    As with GHC and Stack, Cabal and cabal-install now generate warnings and errors prefixed with error codes of the form `[Cabal-xxxxx]`. These will be documented on https://errors.haskell.org, although very few are as yet.

    This change was actually present in Cabal-3.12.0.0, but was inadvertently omitted from the changelog.

### Other changes

- Don't recommend deprecated/removed `extensions` field [#10042](https://github.com/haskell/cabal/issues/10042) [#10044](https://github.com/haskell/cabal/pull/10044)

  When applicable, field `default-extensions` is recommended (rather than
  deprecated/removed `extensions:`).

- Make `Setup copy` and `Setup install` succeed when there's no executable or library [#6750](https://github.com/haskell/cabal/issues/6750) [#9926](https://github.com/haskell/cabal/pull/9926)

  Historically the Setup copy and install steps would fail if the package didn't
  contain an executable or library component. In this case there's nothing to do.

  This required workarounds for downstream users of Cabal to handle this edge case.
  Now that this error has been downgraded to a warning, Cabal will succeed if
  there's nothing to do.

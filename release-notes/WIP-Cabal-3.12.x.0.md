Cabal 3.12.1.0 changelog and release notes.

This file will be edited and the changes incorprated into the official
3.12.1.0 Cabal and Cabal-syntax release notes.

---

### Significant changes

- Deprecation of the `__HADDOCK_VERSION__` macro:
    In the next major version of Cabal, we no longer define the
    `__HADDOCK_VERSION__` macro when invoking GHC through Haddock, since doing
    so essentially guarantees recompilation during documentation generation. We
    audited all uses of `__HADDOCK_VERSION__` in hackage, ensuring there was a
    reasonable path forward to migrate away from using `__HADDOCK_VERSION__` for
    each, while generating the same documentation as it did before.  If you are
    a user of `__HADDOCK_VERSION__`, please take a look at the discussion in
    https://github.com/haskell/cabal/pull/9177 and reach out to us if your use
    case is not covered.

# Revision history for cabal-install-internal

## 3.1900.0

* Split the internal implementation modules out of `cabal-install` (mirroring the earlier `cabal-install-solver` split). This package provides no stability guarantees.
* The CLI layer of every command (the `CommandUI` definitions and option parsing) lives here as well; the `Distribution.Client.API.Command.*` modules in `cabal-install` are pure re-export facades over these entry points.
* Versioned per the `ghc-internal` scheme: `cabal-install X.Y.Z.W` maps to `cabal-install-internal X.(Y*100+Z).C`, where `C` is this package's own additive-patch counter, starting at `0` per tool release series.

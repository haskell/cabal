Cabal 3.6.0.0 Changelog
---


- Added fields :pkg-field:`extra-libraries-static` and
  :pkg-field:`extra-lib-dirs-static` to allow Haskell libraries to remember
  linker flags needed for fully static linking of system libraries into executables. [#7536](https://github.com/haskell/cabal/pull/7536)

- The existing field :pkg-field:`pkgconfig-depends` can used to append the relevant
  output of ``pkg-config --libs --static`` to these new fields automatically.
  When :pkg-field:`extra-libraries-static` is not given, it defaults to
  :pkg-field:`extra-libraries`. When :pkg-field:`extra-lib-dirs-static` is not
  given, it defaults to :pkg-field:`extra-lib-dirs`. [#7536](https://github.com/haskell/cabal/pull/7536)

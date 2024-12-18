## cabal-install and cabal-install-solver 3.14.1.0 changelog

- Fix a bug that causes `cabal init` to crash if `git` is not installed [#8478](https://github.com/haskell/cabal/issues/8478) [#10484](https://github.com/haskell/cabal/issues/10484) [#10486](https://github.com/haskell/cabal/pull/10486)

  - `cabal init` tries to use `git config` to guess the user's name and email.
    It no longer crashes if there is no executable named `git` on `$PATH`.

- Print out which project file we are using with the default verbosity [#8519](https://github.com/haskell/cabal/issues/8519) [#10507](https://github.com/haskell/cabal/pull/10507)

  - Many people have been burnt by cabal catching stray project files located up
    the directory tree. This change tries to protect them at the expense of
    producing more output by default. In particular, before this change, you could
    see which project file is in use by supplying `-v` (the verbose mode), and
    after the change we print this information with the default verbosity.
    Changing the behaviour of cabal is out of scope of this change, and will
    hopefully be done in the future versions (see #9353 for a way forward).

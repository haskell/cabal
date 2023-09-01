cabal-install and cabal-install-solver 3.10.2.0 changelog and release notes
---

## Release 3.10.2.0 is strictly a bug-fix release, with the fixes listed below

- Fix parsing of password-command option [#9002](https://github.com/haskell/cabal/pull/9002)

  The password-command option did not parse its value correctly.
  Quotes were ignored, making many kinds of commands impossible to
  express (e.g.  `sh -c "foo | bar"`).  Also, `cabal user-config`
  treated the argument list as a *list of option values*, rather than a
  *value that is a list*.  As a consequence, `cabal user-config
  update` corrupted the value in the config file.

  Fixed these issues by parsing the command as a space separated list
  of tokens (which may be enclosed in double quotes), and treating the
  parsed list-of-token as one value (not multiple).

- Existence of $XDG_CONFIG_HOME/cabal/config now overrides existence of $HOME/.cabal [#8577](https://github.com/haskell/cabal/issues/8577)

  To avoid pre-XDG backwards compatibility from triggering due to other
  tools accidentally creating a $HOME/.cabal directory, the presence of
  $XDG_CONFIG_HOME/cabal/config now disables pre-XDG backwards
  compatibility.  Presumably $XDG_CONFIG_HOME/cabal/config will never be
  created by accident.

- Shorten script-builds paths [#8841](https://github.com/haskell/cabal/issues/8841) [#8898](https://github.com/haskell/cabal/pull/8898)

  - Use Base64 hash truncated to 26 chars for script-build cache directories.
  - Use the cache directory as the dist directory.
  - Use script-<your-sanitized-script-name> as the component name instead of cabal-script-<...>.
  - Use cabal-script-<your-actual-script-name> for the executable name.
  - This change is incompatible with previous cabal versions in terms of cache location,
    you should manually remove your old caches once you no longer need them.

- Don't add `extra-prog-path: ~/.local/bin` when initially creating `~/.config/cabal/config` [#8951](https://github.com/haskell/cabal/issues/8951)

- Use compiler flags for caching project config [#8819](https://github.com/haskell/cabal/pull/8819)

  This ensures that cached project configs with conditionals re-execute the conditional logic when the compiler changes.

- Fix default Nix configuration option in generated ~/.cabal/config file [#8878](https://github.com/haskell/cabal/pull/8878)

  Fixes the default for ~/.cabal/config file. The nix option should now be commented out by default.

- add base to cabal install --lib default env file [#8903](https://github.com/haskell/cabal/pull/8903)

  This adds base by default to the env file created by `cabal install --lib`. Further it ensures that packagedbs have been created before adding them to the env file.

- Do not check PVP on internal targets [#8361](https://github.com/haskell/cabal/issues/8361) [#9004](https://github.com/haskell/cabal/pull/9004)

  - `cabal check` will not check for dependencies upper bounds in internal
    targets (i.e. test-suites and benchmarks)

- Add new Hackage root keys to bootstrap set [#9068](https://github.com/haskell/cabal/pull/9068)

  The two new [Hackage root keyholders](https://github.com/haskell-infra/hackage-root-keys/tree/master/root-keys) were added to the bootstrap set.

  - Added Hackage root key for Joachim Breitner
  - Added Hackage root key for Mathieu Boespflug

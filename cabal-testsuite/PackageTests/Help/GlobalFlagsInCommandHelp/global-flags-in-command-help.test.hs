import Test.Cabal.Prelude

main = cabalTest $ do
  -- The help of an individual v2 command should mention the global flags,
  -- but not the version flags: they act on the whole `cabal` invocation,
  -- the command they precede is ignored.
  buildHelp <- cabal' "build" ["--help"]
  assertOutputContains "Global flags (must be given before the COMMAND):" buildHelp
  assertOutputContains "--store-dir=DIR" buildHelp
  assertOutputDoesNotContain "--numeric-version" buildHelp

  -- `cabal help COMMAND` shows the same help text, followed by the config
  -- file epilogue it has always printed.
  buildHelp2 <- cabal_raw' ["help", "build"] Nothing
  assertOutputContains "--store-dir=DIR" buildHelp2
  assertOutputContains "You can edit the cabal configuration file to set defaults:" buildHelp2

  -- `cabal path` has its own `--store-dir` and `--config-file` flags, so the
  -- global flags section does not repeat them.
  pathHelp <- cabal' "path" ["--help"]
  assertOutputContains "Global flags (must be given before the COMMAND):" pathHelp
  assertOutputDoesNotContain "--store-dir=DIR              # The location of the build store" pathHelp

  -- Commands that do not use the global configuration do not list it.
  asSetupHelp <- cabal' "act-as-setup" ["--help"]
  assertOutputDoesNotContain "Global flags" asSetupHelp
  formatHelp <- cabal' "format" ["--help"]
  assertOutputDoesNotContain "Global flags" formatHelp
  manHelp <- cabal' "man" ["--help"]
  assertOutputDoesNotContain "Global flags" manHelp

  -- The legacy v1 commands do not list the global flags either: options
  -- such as `--store-dir` only affect the v2 commands.
  v1BuildHelp <- cabal' "v1-build" ["--help"]
  assertOutputDoesNotContain "Global flags" v1BuildHelp

  -- For comparison, the global help has always listed `--store-dir`.
  globalHelp <- cabal' "--help" []
  assertOutputContains "--store-dir=DIR" globalHelp

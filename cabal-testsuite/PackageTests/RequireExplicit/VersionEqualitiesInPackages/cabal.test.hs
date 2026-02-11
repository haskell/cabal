import Test.Cabal.Prelude


main = do
  skipIfOSX "macOS has a different solver output format"
  -- - - other-lib-1.0 (lib) (requires build)
  -- - - some-exe-1.0 (exe:some-exe) (requires build)
  --   - some-lib-1.0 (lib) (requires build)
  -- + - some-exe-1.0 (exe:some-exe) (requires build)
  -- + - other-lib-1.0 (lib) (requires build)
  --   - b-0 (lib) (first run)
  --   - a-0 (lib) (first run)
  cabalTest . withRepo "repo" $ do
    let opts = [ "all", "--dry-run" ]
    cabal "build" $ opts
    cabal "build" $ "--reject-unconstrained-dependencies=all" : opts
    cabal "build" $ "--reject-unconstrained-dependencies=eq" : opts

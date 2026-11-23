import Test.Cabal.Prelude

-- See #4332, dep solving output is not deterministic

assertErr x = assertOutputContains ("not a user-provided goal nor mentioned as a constraint when reject-unconstrained-dependencies=" ++ x)

constraint x = "--constraint=" ++ x

main = do
  skipIfOSX "macOS has a different solver output format"
  -- - - other-lib-1.0 (lib) (requires build)
  -- - - some-exe-1.0 (exe:some-exe) (requires build)
  --   - some-lib-1.0 (lib) (requires build)
  -- + - some-exe-1.0 (exe:some-exe) (requires build)
  -- + - other-lib-1.0 (lib) (requires build)
  --   - b-0 (lib) (first run)
  --   - a-0 (lib) (first run)
  cabalTest' "multipkg-all" . withRepo "repo" $ do
    let opts =
          [ "--dry-run"
          , "--reject-unconstrained-dependencies=all"
          ]
        someLib = constraint "some-lib ==1.0"
        otherLib = constraint "other-lib ==1.0"
        someExe = constraint "some-exe ==1.0"
        otherLibAny = constraint "other-lib -any"
        someExeAny = constraint "some-exe -any"
        no target cs = assertErr "all" =<< fails (cabal' "build" (target : opts ++ cs))
        go target cs = cabal "build" (target : opts ++ cs)

    -- other-lib is a dependency of b, but it's not listed
    no "all" [someLib, someExe]

    -- and some-exe is a build-tool dependency of b, again not listed
    no "all" [someLib, otherLib]

    -- everything's listed, but -any is not a version constraint accepted by
    -- reject-unconstrained-dependencies=all
    no "all" [someLib, otherLibAny, someExeAny]

    -- everything's listed, good to go if we give proper version constraints
    go "all" [someLib, otherLib, someExe]

    -- a depends on b, but b is a local dependency, so it gets a pass, but we
    -- still need to provide version constraints for the other dependencies
    no "a" [someLib, otherLibAny, someExeAny]

    -- a depends on b, but b is a local dependency, so it gets a pass and we're
    -- good to go if we give proper version constraints
    go "a" [someLib, otherLib, someExe]

  cabalTest' "multipkg-eq" . withRepo "repo" $ do
    -- all the options except for those about some-lib
    let opts =
          [ "all"
          , "--dry-run"
          , "--reject-unconstrained-dependencies=eq"
          , constraint "other-lib ==1.0"
          , constraint "some-exe ==1.0"
          ]
        no cs = assertErr "eq" =<< fails (cabal' "build" (opts ++ cs))
        go cs = cabal "build" (opts ++ cs)

    -- everything's listed as == constraint
    go [constraint "some-lib ==1.0"]

    -- everything's listed as == constraint when normalised
    go
      [ constraint "some-lib ==1.0"
      , constraint "some-lib >0"
      , constraint "some-lib <=2"
      ]

    -- not everything's listed as == constraint when normalised
    no
      [ constraint "some-lib >0"
      , constraint "some-lib <=2"
      ]

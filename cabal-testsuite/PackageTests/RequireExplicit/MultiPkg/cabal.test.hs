import Test.Cabal.Prelude

-- See #4332, dep solving output is not deterministic

assertErr x = assertOutputContains ("not a user-provided goal nor mentioned as a constraint, but reject-unconstrained-dependencies=" ++ x ++ " was set")

constraint x = "--constraint=" ++ x

main = do
  cabalTest' "multipkg-all" . withRepo "repo" $ do
    let opts =
          [ "--dry-run"
          , "--reject-unconstrained-dependencies=all"
          ]

    -- other-lib is a dependency of b, but it's not listed in cabal.project
    res <- fails $ cabal' "build" ("all" : opts ++ [constraint "some-exe -any"])
    assertErr "all" res

    -- and some-exe is a build-tool dependency of b, again not listed
    res <- fails $ cabal' "build" ("all" : opts ++ [constraint "other-lib -any"])
    assertErr "all" res

    -- everything's listed, good to go
    cabal "build" $
      "all"
        : opts
        ++ [ constraint "other-lib -any"
           , constraint "some-exe -any"
           ]

    -- a depends on b, but b is a local dependency, so it gets a pass
    cabal "build" $
      "a"
        : opts
        ++ [ constraint "other-lib -any"
           , constraint "some-exe -any"
           ]

  cabalTest' "multipkg-eq" . withRepo "repo" $ do
    -- all the options except for those about some-lib
    let opts =
          [ "all"
          , "--dry-run"
          , "--reject-unconstrained-dependencies=eq"
          , constraint "other-lib ==1.0"
          , constraint "some-exe ==1.0"
          ]

    -- everything's listed as == constraint
    cabal "build" (opts ++ [constraint "some-lib ==1.0"])

    -- everything's listed as == constraint when normalised
    cabal "build" $
      opts
        ++ [ constraint "some-lib ==1.0"
           , constraint "some-lib >0"
           , constraint "some-lib <=2"
           ]

    -- not everything's listed as == constraint when normalised
    res <-
      fails . cabal' "build" $
        opts
          ++ [ constraint "some-lib >0"
             , constraint "some-lib <=2"
             ]
    assertErr "eq" res

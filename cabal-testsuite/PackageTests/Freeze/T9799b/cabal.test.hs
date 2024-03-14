import Test.Cabal.Prelude
main = cabalTest $ do
  expectBroken 9799 $ withRepo "repo" $ do

    -- Show how using 'any' qualifiers always with relaxed bounds can violate that
    --    cabal freeze --constraint=... && cabal build
    -- should be equal to
    --    cabal build --constraint=...
    --
    -- Therefore, the packages in a cabal.project.freeze file must be properly qualified

    out1 <- cabal' "v2-build" ["--constraint='setup.libA == 0.1.0.0'"]
    assertOutputContains "Setup: libA-0.1.0.0" out1
    assertOutputContains "Build: libA-0.2.0.0" out1

    cabal "v2-freeze" ["--constraint=setup.libA == 0.1.0.0'"]
    out2 <- cabal' "v2-build" []
    assertOutputContains "Setup: libA-0.1.0.0" out2
    assertOutputContains "Building: libA-0.2.0.0" out2

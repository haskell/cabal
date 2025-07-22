import Test.Cabal.Prelude
main = cabalTest $ do
  withRepo "repo" $ do

    -- Show how using 'any' qualifiers always with relaxed bounds can violate that
    --    cabal freeze --constraint=... && cabal build
    -- should be equal to
    --    cabal build --constraint=...
    --
    -- Therefore, the packages in a cabal.project.freeze file must be properly qualified

    out1 <- cabal' "v2-build" ["--constraint=setup.libA == 0.1.0.0"]
    assertOutputContains "Setup: libA-0.1.0.0" out1
    assertOutputContains "Building: libA-0.2.0.0" out1

    cabal "v2-freeze" ["--constraint=setup.libA == 0.1.0.0"]

    expectBroken 9799 $ do -- fails when building
      out2 <- cabal' "v2-build" []
      -- After #9799 is fixed, these two lines should be changed to `assertOutputContains`
      assertOutputDoesNotContain "Setup: libA-0.1.0.0" out2
      assertOutputDoesNotContain "Building: libA-0.2.0.0" out2

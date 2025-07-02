import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
    cabal "v2-freeze" []
    cwd <- fmap testCurrentDir getTestEnv
    -- Guarantee that freeze writes scope-qualified constraints, not 'any'
    -- qualified constraints.
    assertFileDoesNotContain (cwd </> "cabal.project.freeze") "any.libA"
    assertFileDoesContain (cwd </> "cabal.project.freeze") "setup.libA == 0.2.0.0"

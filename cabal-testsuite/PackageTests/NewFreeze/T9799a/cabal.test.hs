import Test.Cabal.Prelude
main = cabalTest $ do
  withRepo "repo" $ do
    cabal "v2-freeze" []
    cwd <- fmap testCurrentDir getTestEnv
    -- Guarantee that freeze writes scope-qualified constraints, not 'any'
    -- qualified constraints.
    expectBroken 9799 $ do
      assertFileDoesNotContain (cwd </> "cabal.project.freeze") "any.libA"
      assertFileDoesContain (cwd </> "cabal.project.freeze") "libA == 0.1.0.0"
      assertFileDoesContain (cwd </> "cabal.project.freeze") "setup.libA == 0.2.0.0"

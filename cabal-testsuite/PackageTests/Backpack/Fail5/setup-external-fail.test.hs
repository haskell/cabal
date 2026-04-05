import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    withPackageDb $ do
      -- Install framework WITHOUT instantiation (simulates nix callCabal2nix)
      recordMode DoNotRecord $
        withDirectory "repo/framework-0.1.0.0" $ setup_install []
      -- Install the App implementation package separately
      recordMode DoNotRecord $
        withDirectory "repo/app-impl-0.1.0.0" $ setup_install []
      -- Configure consumer — should fail because the instantiated
      -- framework (with App=app-impl:App) was never built.
      -- The exact error message is checked via the .out file.
      withDirectory "repo/consumer-0.1.0.0" $ do
        fails $ setup' "configure" []
        return ()

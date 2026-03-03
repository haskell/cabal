import Test.Cabal.Prelude
main = do
  -- TODO: this might be a GHC bug that needs fixing
  skipIfAlpine "bug #11041"
  setupAndCabalTest $ do
    skipUnlessGhcVersion ">= 8.1"
    setup "configure" []
    setup "build" []

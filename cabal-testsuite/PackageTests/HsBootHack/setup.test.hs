import Test.Cabal.Prelude

-- Test the hs-boot hack respects working directory
main = setupTest . recordMode DoNotRecord $ withDirectory "dep" $ do
 void $ setup' "configure" []
 void $ setup' "build" []

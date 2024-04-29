import Test.Cabal.Prelude
main = cabalTest $ recordMode DoNotRecord $
  cabal "v1-install" []

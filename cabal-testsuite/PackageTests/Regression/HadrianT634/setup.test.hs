import Test.Cabal.Prelude
import Test.Cabal.Script
main = setupTest $
  withDirectory "pkg" $
  void $ setup' "configure" []

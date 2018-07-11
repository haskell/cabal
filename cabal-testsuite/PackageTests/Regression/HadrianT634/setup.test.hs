import Test.Cabal.Prelude
import Test.Cabal.Script
main = setupTest $
  void $ setup'' "pkg" "configure" ["--cabal-file", "pkg/a.cabal"]

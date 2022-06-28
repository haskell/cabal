import Test.Cabal.Prelude

-- No exposed modules.
main = cabalTest $
  cabal "check" []

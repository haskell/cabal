import Test.Cabal.Prelude

-- Relative filepath outside source tree.
main = cabalTest $
  fails $ cabal "check" []

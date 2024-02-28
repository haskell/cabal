import Test.Cabal.Prelude

-- Relative filepath outside source tree.
main = cabalTest $
  fails $ withDirectory "RelativeOutsideInner" $ cabal "check" []

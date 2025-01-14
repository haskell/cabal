import Test.Cabal.Prelude

-- `custom-setup` bounds.
main = cabalTest $
  fails $ cabal "check" []

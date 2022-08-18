import Test.Cabal.Prelude

-- Non-regonised (head, this, etc.) repo.
main = cabalTest $
  fails $ cabal "check" []

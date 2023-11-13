import Test.Cabal.Prelude

-- `check` should not be confused by an user flag.
main = cabalTest $
  fails $ cabal "check" []

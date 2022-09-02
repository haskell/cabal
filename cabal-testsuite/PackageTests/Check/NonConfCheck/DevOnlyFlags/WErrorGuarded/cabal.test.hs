import Test.Cabal.Prelude

-- Do not complain if WError is under a user, off-by-default flag.
main = cabalTest $
  cabal "check" []

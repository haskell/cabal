import Test.Cabal.Prelude

-- `tag` needed in `this` repos.
main = cabalTest $
  fails $ cabal "check" []

import Test.Cabal.Prelude

-- Points to dist.
main = cabalTest $
  fails $ cabal "check" []

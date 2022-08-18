import Test.Cabal.Prelude

-- Invalid local paths.
main = cabalTest $
  fails $ cabal "check" []

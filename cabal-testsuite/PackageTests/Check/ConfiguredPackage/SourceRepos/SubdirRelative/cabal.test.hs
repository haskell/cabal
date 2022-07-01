import Test.Cabal.Prelude

-- `subdir` is not a relative path.
main = cabalTest $
  fails $ cabal "check" []

import Test.Cabal.Prelude

-- `subdir` is not a good relative path.
main = cabalTest $
  fails $ cabal "check" []

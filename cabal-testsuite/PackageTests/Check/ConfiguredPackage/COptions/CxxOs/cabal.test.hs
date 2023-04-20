import Test.Cabal.Prelude

-- `cxx-options`, do not use `-O1`.
main = cabalTest $
  cabal "check" []

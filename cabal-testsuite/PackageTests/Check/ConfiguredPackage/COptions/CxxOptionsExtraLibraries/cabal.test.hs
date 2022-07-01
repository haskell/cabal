import Test.Cabal.Prelude

-- `cxx-options`, use `extra-libraries` instead of `-l`.
main = cabalTest $
  fails $ cabal "check" []

import Test.Cabal.Prelude

-- `cc-options`, use `include-dirs` instead of `-I`.
main = cabalTest $
  fails $ cabal "check" []

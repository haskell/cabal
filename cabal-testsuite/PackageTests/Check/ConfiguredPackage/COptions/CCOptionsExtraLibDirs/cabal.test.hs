import Test.Cabal.Prelude

-- `cc-options`, use `extra-lib-dirs` instead of `-L`.
main = cabalTest $
  fails $ cabal "check" []

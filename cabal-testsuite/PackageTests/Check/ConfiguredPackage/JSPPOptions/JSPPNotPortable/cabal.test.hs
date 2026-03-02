import Test.Cabal.Prelude

-- `jspp-options`, do not use use non portable flags.
main = cabalTest $
  fails $ cabal "check" []

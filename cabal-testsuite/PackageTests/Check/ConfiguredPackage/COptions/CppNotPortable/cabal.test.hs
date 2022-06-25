import Test.Cabal.Prelude

-- `cpp-options`, do not use use non portable flags.
main = cabalTest $
  fails $ cabal "check" []

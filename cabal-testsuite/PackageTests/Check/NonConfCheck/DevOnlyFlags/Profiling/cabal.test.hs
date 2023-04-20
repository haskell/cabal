import Test.Cabal.Prelude

-- Profiling flags unsuited for distribution.
main = cabalTest $
  cabal "check" []

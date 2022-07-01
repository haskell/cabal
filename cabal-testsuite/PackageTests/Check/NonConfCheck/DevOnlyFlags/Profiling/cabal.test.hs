import Test.Cabal.Prelude

-- Profiling flags unsuited for distribution.
main = cabalTest $
  fails $ cabal "check" []

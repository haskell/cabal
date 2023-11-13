import Test.Cabal.Prelude

-- Straddle deps declarations (build-depends: base > 5, base < 6)
-- should not error.
main = cabalTest $
  cabal "check" []

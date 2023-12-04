import Test.Cabal.Prelude

-- Output warning when an -O2 outside a cabal flag, along with one inside.
main = cabalTest $ cabal "check" []

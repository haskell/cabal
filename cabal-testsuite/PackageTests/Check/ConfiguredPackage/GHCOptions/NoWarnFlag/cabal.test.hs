import Test.Cabal.Prelude

-- Do not output warning when an -O2 is behind a cabal flag.
main = cabalTest $ cabal "check" []

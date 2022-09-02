import Test.Cabal.Prelude

-- Output warning when an -O2 inside a cabal flag, but the flag is not
-- marked as `manual: True`.
main = cabalTest $ cabal "check" []

import Test.Cabal.Prelude

-- Issue #8507: trailing space in `default-language` should not make
-- `cabal build` complain.
main = cabalTest $ cabal "v2-build" ["all"]


import Test.Cabal.Prelude

-- Warns (but does not error) when an ignore option is not recognised.
main = cabalTest $ cabal "check" ["--ignore=foo"]

import Test.Cabal.Prelude

-- Should ignore warnings if instructed so.
main = cabalTest $ cabal "check" ["--ignore=short-description"]

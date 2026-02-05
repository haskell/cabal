import Test.Cabal.Prelude
-- Test that `cabal haddock --haddock-all` correctly finds the .haddock
-- interface file for an internal sub-library (no "could not find link
-- destinations" warning).
main = cabalTest $ do
    cabal "haddock" ["--haddock-all"]

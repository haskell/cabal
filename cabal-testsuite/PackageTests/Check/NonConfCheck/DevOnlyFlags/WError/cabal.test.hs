import Test.Cabal.Prelude

-- WError without -W/-Wall.
main = cabalTest $
  fails $ cabal "check" []

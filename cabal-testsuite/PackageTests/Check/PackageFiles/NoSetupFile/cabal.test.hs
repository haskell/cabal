import Test.Cabal.Prelude

-- No Setup.hs/lhs.
main = cabalTest $
  fails $ cabal "check" []

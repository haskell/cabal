import Test.Cabal.Prelude

-- `signatures` field used with cabal-version < 2.0
main = cabalTest $
  fails $ cabal "check" []

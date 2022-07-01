import Test.Cabal.Prelude

-- Missing license file.
main = cabalTest $
  fails $ cabal "check" []

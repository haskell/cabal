import Test.Cabal.Prelude

-- Debug flags are inappropriate for release.
main = cabalTest $
  fails $ cabal "check" []

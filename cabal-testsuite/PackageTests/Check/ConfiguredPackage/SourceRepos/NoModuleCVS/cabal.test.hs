import Test.Cabal.Prelude

-- No `module` (CVS only).
main = cabalTest $
  fails $ cabal "check" []

import Test.Cabal.Prelude

-- cabal-version <2.2, Paths_pkg *and* `default extensions` w/
-- RebindableSyntax plus OverloadedList or similar do not get well
-- together.
main = cabalTest $
  fails $ cabal "check" []

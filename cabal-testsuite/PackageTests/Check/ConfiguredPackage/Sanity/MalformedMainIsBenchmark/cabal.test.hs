import Test.Cabal.Prelude

-- `main-is` has to be a `.hs` or `.lhs` file (or C* source file) (benchmark).
main = cabalTest $
  fails $ cabal "check" []

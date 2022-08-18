import Test.Cabal.Prelude

-- Mismatched package name/filename.
main = cabalTest $
  fails $ cabal "check" []

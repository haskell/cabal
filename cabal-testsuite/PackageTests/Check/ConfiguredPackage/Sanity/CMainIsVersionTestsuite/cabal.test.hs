import Test.Cabal.Prelude

-- You need `cabal-version` ≥ 1.18 to use C/C++/obj-C source files
-- in `main-is`. (testsuite)
main = cabalTest $
  fails $ cabal "check" []

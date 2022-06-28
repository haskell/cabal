import Test.Cabal.Prelude

-- All `autogen-includes` should appear in `install-includes` or
-- `includes` (testsuite).
main = cabalTest $
  fails $ cabal "check" []

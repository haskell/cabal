import Test.Cabal.Prelude

-- All `autogen-modules` have to appear in `other-modules` or
-- `exposed-modules` (testsuite).
main = cabalTest $
  fails $ cabal "check" []

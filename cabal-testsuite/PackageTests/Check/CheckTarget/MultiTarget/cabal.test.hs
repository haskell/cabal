import Test.Cabal.Prelude

-- `cabal check` works when passing multiple targets to check.
main = cabalTest $ do
  fails $ cabal "check" ["pkg-a", "pkg-b"]


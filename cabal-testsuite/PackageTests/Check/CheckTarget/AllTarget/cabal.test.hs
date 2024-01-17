import Test.Cabal.Prelude

-- `cabal check` works when passing `all`.
main = cabalTest $ do
  cabal "check" ["all"]


import Test.Cabal.Prelude
-- Test that `cabal haddock` doesn't require explicit
-- `--enable-dependencies` to happily process links to external packages.
-- In this example package B depends on an external package A.
main = cabalTest . withRepo "repo" $
    cabal "haddock" ["B"]

import Test.Cabal.Prelude
main = cabalTest $ do
        fails $ cabal "v2-run" ["--ghc-options=-Wall -Werror", "--enable-documentation"]
